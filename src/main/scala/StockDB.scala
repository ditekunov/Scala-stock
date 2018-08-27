package StockDB

import ClientHandler.ClientHandler
import com.typesafe.scalalogging.LazyLogging
import oper._
import slick.dbio.DBIOAction

import scala.concurrent.Future
import scala.concurrent.Future._
import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.SQLiteProfile.api._

import collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import slick.lifted.Rep
import slick.lifted.FunctionSymbolExtensionMethods._
object StockTables {
  class Clients(tag: Tag) extends Table[(String, Double)](tag, "CLIENTS") {
    def name = column[String]("NAME", O.PrimaryKey)
    def balance = column[Double]("PRICE")
    def * = (name, balance)
    // A reified foreign key relation that can be navigated to create a join
  }
  val clients = TableQuery[Clients]
  class ClientsToCurrencies(tag : Tag) extends Table[(String, String, Int)](tag, "CLIENTS_TO_CURRENCIES") {
    def clientName = column[String]("CLIENT_NAME")
    def currencyName = column[String]("CURRENCY_NAME")
    def currencyCount = column[Int]("CURRENCY_COUNT")
    def * = (clientName, currencyName, currencyCount)

    def client = foreignKey("CLIENT_FK", clientName, clients)(_.name)
  }
  val clientsToCurrencies = TableQuery[ClientsToCurrencies]
  class OperationRequests(tag : Tag) extends Table[(String, Int, String, String, Int, Double)](tag, "OPERATION_REQUESTS") {
    def operationType = column[String]("OPERATION_TYPE")
    def timeIndex = column[Int]("TIME_INDEX")
    def clientName = column[String]("CLIENT_NAME")
    def currencyName = column[String]("CURRENCY_NAME")
    def currencyCount = column[Int]("CURRENCY_COUNT")
    def currencyPrice = column[Double]("CURRENCY_RATE")
    def * = (operationType, timeIndex, clientName, currencyName, currencyCount, currencyPrice)

    def client = foreignKey("CLIENT_FK", clientName, clients)(_.name)
  }
  val operationRequests = TableQuery[OperationRequests]
}

object StockDBMethods {
  import StockTables._
  def clearClientsReq() = {
    clients.delete
  }
  def clearClientsToCurrenciesReq() = {
    clientsToCurrencies.delete
  }
  def clearOperationRequestsReq() = {
    operationRequests.delete
  }
  def clearTablesReq() = {
    clearClientsReq() >> clearClientsToCurrenciesReq() >> clearOperationRequestsReq()
  }

  def clearTables(implicit db : Database) = {
    db.run(clearTablesReq)
  }

  def addClientReq(client : ClientHandler) = {
    (clients += (client.name, client.balance)) >> (clientsToCurrencies ++= client.currencies.map(p => (client.name, p._1, p._2)))
  }
  def removeClientReq(name : String) = {
    clients.filterNot(_.name === name).delete
  }
  def getClientRowReq(name : String) = {
     clients.filter(_.name === name).result.head
  }
  def getClientCurrenciesReq(name : String) = {
    clientsToCurrencies.filter(_.clientName === name).result
  }

  def addClient(client : ClientHandler)(implicit db : Database) = {
    db.run(addClientReq(client))
  }
  def removeClient(name : String)(implicit db : Database) = {
    db.run(removeClientReq(name))
  }
  def getClient(name : String)(implicit db : Database) = {
    val client = db.run(getClientRowReq(name))
    val clientCurrenciesList = db.run(getClientCurrenciesReq(name))
    val clientCurrenciesMap = clientCurrenciesList.map(curs => {
      curs.toList.map(oneCur => oneCur._2 -> oneCur._3).toMap
    })
    Future.sequence(List(client, clientCurrenciesMap)).map(p => {
      val clientPair = p.head match {
        case t : (String, Double) => t
        case _ => throw new Exception
      }
      val currencies = p(1) match {
        case m : Map[String, Int] => m
        case _ => throw new Exception
      }
      new ClientHandler(clientPair._1, clientPair._2, mutable.Map.empty ++ currencies)
    })
  }
  def getClients(implicit db : Database) = {
    val namesReq = for (
      client <- clients
    ) yield client.name
    db.run(namesReq.result).map(l => {
      Future.sequence(l.map(s => getClient(s)).toList)
    }).flatten
  }
  def addCurrency(currency : String, startCount : Int = 0)(implicit db : Database) = {
    val namesReq = for (
      client <- clients
    ) yield client.name
    db.run(namesReq.result).map(l => {
      db.run(clientsToCurrencies ++= l.map(s => (s, currency, startCount)))
    }).flatten
  }
  def removeCurrency(currency : String)(implicit db : Database) = {
    db.run(clientsToCurrencies.filter(_.currencyName === currency).delete)
  }
  def getLatestTimeIndReq =
    operationRequests.sortBy(_.timeIndex.desc).result.headOption.map(c => c.map(_._2).getOrElse(0))
  def addOperationRequest(op : QueuedOperation)(implicit db : Database) = {
    db.run(getLatestTimeIndReq).map(t => {
      op match {
        case p : Selling => db.run(operationRequests += ("sell", t + 1, op.name, op.currency, op.count, op.price))
        case p : Purchase => db.run(operationRequests += ("buy", t + 1, op.name, op.currency, op.count, op.price))
      }
    }).flatten
  }
  def getOperationRequests(implicit db : Database) = {
    db.run(operationRequests.sortBy(_.timeIndex.asc).result.map(l => l.map(f => {
      f._1 match {
        case "sell" => Selling(f._3, f._4, f._5, f._6)
        case "buy" => Purchase(f._3, f._4, f._5, f._6)
      }
    }).toList))
  }
  def rewriteOperationRequests(l : Iterable[QueuedOperation])(implicit db: Database) = {
    db.run(operationRequests.delete) transformWith { _ =>
      val futures = l.foldLeft((List[Future[Any]](),0))((pair, op) => {
        op match {
          case p : Selling =>
            (
              pair._1 :+ db.run(operationRequests += ("sell", pair._2, op.name, op.currency, op.count, op.price)),
              pair._2 + 1
            )
          case p : Purchase =>
            (
              pair._1 :+ db.run(operationRequests += ("buy", pair._2, op.name, op.currency, op.count, op.price)),
              pair._2 + 1
            )
        }
      })._1
      Future.sequence(futures)
    }
  }
  def rewriteClients(l : Iterable[ClientHandler])(implicit db : Database) = {
    db.run(operationRequests.delete) transformWith { _ =>
      Future.sequence(l.map(c => addClient(c)))
    }
  }
  def changeClientBalance(name : String, balance : Double)(implicit db: Database) = {
    db.run(clients.filter(_.name===name).map(_.balance).update(balance))
  }
  def changeClientCurrency(name: String, currency : String, count : Int)(implicit db : Database) = {
    db.run(clientsToCurrencies.filter(c => c.clientName === name && c.currencyName === currency).map(_.currencyCount).update(count))
  }
}

object DBMain extends App with LazyLogging{
  import StockDBMethods._
  implicit val db = Database.forURL(
    "jdbc:sqlite:C:\\Users\\mitya\\Documents\\Git_workspace\\Scala-Stock\\src\\main\\resources\\test.db",
    driver = "org.sqlite.JDBC"
  )
}
