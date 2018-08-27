package serializer

import ClientHandler.ClientHandler
import oper._
import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods.{compact, render, pretty}
import org.json4s.JsonDSL._

object ObjectsToJson {
  def error(exc : Throwable) =
    pretty(render(("successful" -> false) ~ ("error" -> exc.getMessage)))
  def success(comment : Option[String] = Option.empty) = {
    if (comment.isEmpty)
      pretty(render("success" -> true))
    else
      pretty(render(("success" -> true) ~ ("comment" -> comment)))
  }
  private def packClient(client : ClientHandler) = {
    val currenciesStr = client.currencies.mapValues(_.toString).toList
    ("name" -> client.name)~
      ( "balance" -> client.balance.toString)~
      ("currencies" -> currenciesStr)
    
  }
  def clientToJson(client : ClientHandler) = pretty(render(packClient(client)))
  def clientsToJson(clients : Iterable[ClientHandler]) = pretty(render(clients.map(packClient)))
  private def sellBuyPack(name : String, cur : String, count : Int, price : Double) =
    ("name"->name)~("currency"->cur)~("count"->count)~("price"->price)
  private def operationPack(oper : QueuedOperation):JValue =
    oper match {
      case Selling(name, cur, count, price) =>
        ("type" -> "Selling") ~ ( "parameters" -> sellBuyPack(name, cur, count, price))
      case Purchase(name, cur, count, price) =>
        ("type" -> "Selling") ~ ( "parameters" -> sellBuyPack(name, cur, count, price))
      case _ =>
        "type" -> "error"
    }
  def operationToJson(oper : QueuedOperation) :String = pretty(render(operationPack(oper)))
  def operationsToJson(opers : Iterable[QueuedOperation]) : String = pretty(render(opers.map(operationPack)))
}
