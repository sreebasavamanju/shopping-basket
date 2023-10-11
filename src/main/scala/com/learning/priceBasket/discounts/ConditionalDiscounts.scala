package com.learning.priceBasket.discounts

import com.learning.priceBasket.dataTypes.Discount.{ConditionalDiscount, Discount}
import com.learning.priceBasket.dataTypes.{Basket, Good}

object ConditionalDiscounts {

  private def filterDiscounts(
    numberOfGoodsRequired: Map[String, Int],
    numberOfGoodsInBasket: Map[String, Int]
  ): Boolean = {
    numberOfGoodsRequired
      .filter(
        requiredGood =>
          requiredGood._2 <= numberOfGoodsInBasket
            .filter(
              numberOfGood => numberOfGoodsRequired.contains(numberOfGood._1)
            )
            .getOrElse(requiredGood._1, 0)
      )
      .equals(numberOfGoodsRequired)
  }

  /**
    * Checks whether a conditional discount is applicable
    * based on items in the basket, and if so converts to a
    * discount
    */
  def processConditionalDiscounts(
    basket: Basket,
    discounts: List[ConditionalDiscount]
  ): List[Discount] =
    discounts
      .filter { conditionalDiscount =>
        {
          val numberOfGoodRequired: Map[String, Int] =
            conditionalDiscount.condition.countValues

          filterDiscounts(numberOfGoodRequired, basket.countQuantityOfEach)
        }
      }
      .map(d => {
        val numberRequired = d.condition.goodsRequired.size
        val numberInBasket =
          basket.countQuantityOfEach(d.condition.goodsRequired.head)
        Discount(d.item, d.discount, numberInBasket / numberRequired)
      })
}
