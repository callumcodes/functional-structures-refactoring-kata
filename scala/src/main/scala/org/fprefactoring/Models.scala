package org.fprefactoring

object Models {
  case class Amount(value: BigDecimal) extends AnyVal {
    def -(amount: Amount): Amount = Amount(value - amount.value)
    def half = Amount(value / 2)
  }

  sealed trait CustomerId {
    def value: String
  }

  case object GoldCustomer extends CustomerId {
    override val value = "gold-customer"
  }

  case object NormalCustomer extends CustomerId {
    override val value = "normal-customer"
  }

  case object NoCustomer extends CustomerId {
    override val value = ""
  }

  case class CartId(value: String) extends AnyVal


  case class Cart(id: CartId, customerId: CustomerId, amount: Amount)

  class DiscountRule(f: Cart => Amount) extends (Cart => Amount) {
    def apply(c: Cart): Amount = f(c)
  }

  case object HalfDiscount extends DiscountRule(cart => cart.amount.half)

  case object NoDiscount extends DiscountRule(_ => Amount(0))
}
