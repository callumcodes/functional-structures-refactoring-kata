package org.fprefactoring

import org.fprefactoring.Models._

object App {

  def applyDiscount(cartId: CartId, storage: Storage[Cart]): Unit = {
    loadCart(cartId).foreach { cart =>
      val discount = lookupDiscountRule(cart.customerId)
      if(discount != NoDiscount) save(updateAmount(cart, discount(cart)), storage)
    }
  }

  def loadCart(id: CartId): Option[Cart] = {
    val customer = if (id.value.contains("gold")) {
      Some(GoldCustomer)
    } else if (id.value.contains("normal")) {
      Some(NormalCustomer)
    } else
      None

    customer.map(Cart(id, _, Amount(100)))
  }

  def lookupDiscountRule(id: CustomerId): DiscountRule = id match {
    case GoldCustomer => HalfDiscount
    case _ => NoDiscount
  }

  def updateAmount(cart: Cart, discount: Amount): Cart = cart.copy(amount = cart.amount - discount)

  def save(cart: Cart, storage: Storage[Cart]): Unit =
    storage.flush(cart)
}
