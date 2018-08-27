package ClientHandler
import collection.mutable


class ClientHandler(val name : String, var balance : Double = 0, val currencies: mutable.Map[String, Int] = mutable.Map.empty) {
  def sell(currency : String, count : Int, price : Double)= {
    gainBalance(count * price)
    wasteCurrency(currency, count)
    this
  }
  def buy(currency : String, count : Int, price : Double) = {
    wasteBalance(count * price)
    gainCurrency(currency, count)
    this
  }

  def filterCurrencies(newCurs :TraversableOnce[String]) = {
    currencies ++= newCurs.filter(c => !currencies.contains(c)).map(_ -> 0)
    currencies --= currencies.keySet -- newCurs
    this
  }
  def addCurrency(currency : String, startCount : Int = 0)= {
    currencies += currency -> startCount
    this
  }
  def removeCurrency(currency : String, price : Double = 1.0) = {
    balance += currencies(currency) * price
    currencies -= currency
    this
  }
  def updateBalance(money : Double) =
    balance = money
  def updateCurrency(currency : String, count : Int) =
    currencies(currency) = count
  def gainBalance(money : Double) =
    balance += money
  def wasteBalance(money : Double) =
    balance -= money
  def gainCurrency(currency : String, count : Int) =
    currencies.update(currency, currencies(currency) + count)
  def wasteCurrency(currency : String, count : Int) =
    currencies.update(currency, currencies(currency) - count)
  def canSpend(money : Double): Boolean = balance >= money
  def canSell(currency: String, count : Int): Boolean = currencies(currency) >= count

  def get(currency : String) : Int = currencies(currency)

  override def toString: String = s"$name: $balance$$ | $currencies"
}