package cafes

case class Coffee(price: Double = 5)

case class CreditCard(owner: String)

object CreditCard {

  def charge(cc: CreditCard, amount: Double) = {
    println(s"charged ${cc.owner} $$$amount")
  }
}

case class Charge(cc: CreditCard, amount: Double) {

  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards")
}

class Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges)                = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

object Payments {
  def charge(charges: List[Charge]) = charges.foreach(c => CreditCard.charge(c.cc, c.amount))
}

object Main extends App {
  val cafe = new Cafe()

  val card1 = CreditCard("Bill")
  val card2 = CreditCard("John")

  // two different credit cards, each with multiple charges
  val (_, charge1) = cafe.buyCoffees(card1, 3)
  val (_, charge2) = cafe.buyCoffees(card1, 8)

  val (_, charge3) = cafe.buyCoffees(card2, 1)
  val (_, charge4) = cafe.buyCoffees(card2, 1)
  val (_, charge5) = cafe.buyCoffees(card2, 1)

  // now let's coalesce the charges to minimize credit card fees
  val charges = List(charge1, charge2, charge3, charge4, charge5)
  println(charges)

  // where we had 5 charges before, we should now have 5
  val coalesced = cafe.coalesce(charges)
  println(coalesced)

  // everything until here is referentially transparent. In a real example, this is where
  // our state, or impurities, would live
  Payments.charge(coalesced)
}
