package com.learning.priceBasket.dataTypes

object Discount {

  abstract class DiscountGeneric {
    val item: String
    val discount: Double
  }

  case class ConditionalDiscount(item: String,
                                 discount: Double,
                                 condition: Condition)
      extends DiscountGeneric

  case class Discount(item: String, discount: Double, numberOfTimesToApply: Int)
      extends DiscountGeneric

  case class Condition(goodsRequired: List[String]) {
    def countValues: Map[String, Int] =
      this.goodsRequired.groupBy(identity).mapValues(_.size).toMap
  }
}
