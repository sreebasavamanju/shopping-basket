package com.learning.priceBasket.utils

import com.learning.priceBasket.dataTypes.Discount.Discount
import com.learning.priceBasket.dataTypes.{Basket, Good}
import com.learning.priceBasket.discounts.CalculateDiscountedGoods.getTotal

import scala.math.BigDecimal.RoundingMode

object Logging {

  def outputNoOffers(basket: Basket): Unit =
    if (getTotal(basket, discounted = false) == getTotal(
          basket,
          discounted = true
        )) println("(No offers available)")

  def outputDiscount(discount: Discount, oldPrice: Double): Unit = {
    val savings = oldPrice - (oldPrice * (1 - discount.discount))

    println(
      discount.item + " " + (discount.discount * 100) + "% off: " + (BigDecimal(
        savings
      ) * 100)
        .setScale(0, RoundingMode.HALF_EVEN) + "p"
    )
  }

  def outputTotalBasketCost(basket: Basket, total: Boolean): Unit =
    if (total) println(s"Total price: £${getTotal(basket, discounted = true)}")
    else println(s"Subtotal: £${getTotal(basket, discounted = false)}")
}
