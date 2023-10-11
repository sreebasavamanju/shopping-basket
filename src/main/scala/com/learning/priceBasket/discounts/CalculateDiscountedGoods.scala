package com.learning.priceBasket.discounts

import com.learning.priceBasket.dataTypes.Discount.{ConditionalDiscount, Discount}
import com.learning.priceBasket.dataTypes.{Basket, Good}
import com.learning.priceBasket.discounts.ApplyDiscounts._
import com.learning.priceBasket.utils.Logging._
import com.learning.priceBasket.discounts.ConditionalDiscounts.processConditionalDiscounts

import scala.math.BigDecimal.RoundingMode

object CalculateDiscountedGoods {

  def getTotal(basket: Basket, discounted: Boolean): BigDecimal = {
    BigDecimal(
      basket.goods
        .map(good => good.getCost(discounted))
        .sum
    ).setScale(2, RoundingMode.HALF_EVEN)
  }

  def getMaxNumberOfDiscounts(discounts: List[Discount],
                              countOfItems: Map[String, Int]): List[Discount] =
    discounts
      .map(
        discount =>
          if (discount.numberOfTimesToApply > countOfItems(discount.item)) {
            discount.copy(numberOfTimesToApply = countOfItems(discount.item))
          } else {
            discount
        }
      )

  def generateCorrectNumberOfDiscounts(
    discounts: List[Discount],
    mapOfPrices: Map[String, Double],
    countOfItems: Map[String, Int]
  ): List[Discount] = {

    val allDiscounts = for {
      dis <- getMaxNumberOfDiscounts(discounts, countOfItems)
      _ <- 0 until dis.numberOfTimesToApply
    } yield {
      Discount(dis.item, dis.discount, dis.numberOfTimesToApply)
    }
    allDiscounts.foreach(dis => outputDiscount(dis, mapOfPrices(dis.item)))

    allDiscounts
  }

  def calculateDiscountedGoods(initialBasket: Basket,
                               pricesMap: Map[String, Double],
                               conditionalDiscounts: List[ConditionalDiscount],
                               discounts: List[Discount]): Basket = {

    outputTotalBasketCost(initialBasket, total = false)

    val allDiscounts = (discounts ::: processConditionalDiscounts(
      initialBasket,
      conditionalDiscounts
    )) filter (d => initialBasket.goods.map(g => g.name).contains(d.item))

    getBasketWithDiscountsApplied(
      initialBasket,
      generateCorrectNumberOfDiscounts(
        allDiscounts,
        pricesMap,
        initialBasket.goods.map(g => g.name).groupBy(identity).mapValues(_.size).toMap
      )
    )
  }
}
