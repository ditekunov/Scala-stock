package req

import ClientHandler.ClientHandler
import oper._

import scala.util.Try
import collection.mutable

object RequestParser {
  def tryToClient(m : Map[String, Any]) : Try[ClientHandler] = {
    Try(
      new ClientHandler(
        m("name").toString,
        m.getOrElse("balance", 0).toString.toDouble,
        mutable.Map.empty[String, Int] ++ (m - "name" - "balance").mapValues(_.toString.toInt)
      )
    )
  }
  def tryToSelling(m : Map[String, Any]) : Try[Selling] = {
    Try(
      Selling(
        m("name").toString,
        m("currency").toString,
        m.get("count").map(_.toString.toInt).get,
        m.get("price").map(_.toString.toDouble).get
      )
    )
  }
  def tryToPurchase(m : Map[String, Any]) : Try[Purchase] = {
    Try(
      Purchase(
        m("name").toString,
        m("currency").toString,
        m.get("count").map(_.toString.toInt).get,
        m.get("price").map(_.toString.toDouble).get
      )
    )
  }
  def tryToClientRemoval(m : Map[String, Any]) : Try[ClientRemoval] = {
    Try(
      ClientRemoval (
        m("name").toString
      )
    )
  }
  def tryToCurrencyAddition(m : Map[String, Any]) : Try[CurrencyAddition] = {
    Try(
      CurrencyAddition(
        m("currency").toString,
        m.getOrElse("startCount", 0).toString.toInt
      )
    )
  }
  def tryToCurrencyRemoval(m : Map[String, Any]) : Try[CurrencyRemoval] = {
    Try (
      CurrencyRemoval(
        m("currency").toString,
        m.getOrElse("price", 0).toString.toDouble
      )
    )
  }
  def tryToChangingClientBalance(m : Map[String, Any]) : Try[ChangingClientBalance] = {
    Try (
      ChangingClientBalance(
        m("name").toString,
        m("balance").toString.toDouble
      )
    )
  }
  def tryToChangingClientCurrency(m : Map[String, Any]) : Try[ChangingClientCurrency] = {
    Try (
      ChangingClientCurrency(
        m("name").toString,
        m("currency").toString,
        m("count").toString.toInt
      )
    )
  }
}