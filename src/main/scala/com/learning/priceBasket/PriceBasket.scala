package com.learning.priceBasket

import com.learning.priceBasket.dataTypes.Basket
import com.learning.priceBasket.discounts.CalculateDiscountedGoods._
import com.learning.priceBasket.utils.Logging._
import com.learning.priceBasket.utils.ReadFile._

object PriceBasket {

  def main(args: Array[String]): Unit = {

    if (args.length == 0) {
      println("Please specify at least one item")
      System.exit(-1)
    }
    val pricesMap = readCSVToMap("src/main/resources/prices.csv")
    try {
      val basket = Basket.apply(args.toList, pricesMap)

      val conditionalDiscounts = readCSVToConditionalDiscount(
        "src/main/resources/conditional_discounts.csv"
      )

      val discounts =
        readCSVToDiscount(
          "src/main/resources/discounts.csv",
          basket.countQuantityOfEach
        )

      val basketCalculated = calculateDiscountedGoods(
        basket,
        pricesMap,
        conditionalDiscounts,
        discounts
      )

      outputNoOffers(basketCalculated)

      outputTotalBasketCost(basketCalculated, total = true)
    } catch {
      case e: NoSuchElementException => println(e.printStackTrace())
        System.exit(-1)
    }
  }
}
