package com.learning.priceBasket.discounts

import com.learning.priceBasket.dataTypes.Discount.Discount
import com.learning.priceBasket.dataTypes.{Basket, Good}
import com.learning.priceBasket.PriceBasket._

object ApplyDiscounts {

  def applyAllDiscounts(basket: Basket, discounts: List[Discount]): Basket = {
    Basket(
      discounts
        .filter(d => basket.goods.map(g => g.name).contains(d.item))
        .map(dis => {
          val priceGood = basket.goods
            .find(b => { b.name == dis.item && b.price == b.discountedPrice })
            .get
            .price
          Good(dis.item, priceGood, priceGood * (1 - dis.discount))
        })
    )
  }

  def findNumberOfEachItemNotDiscounted(
    basket: Basket,
    discountedGoods: Basket
  ): Map[String, Int] = {

    def howMany(item: String): Int =
      discountedGoods.countQuantityOfEach.getOrElse(item, 0)

    basket.countQuantityOfEach
      .flatMap(h => Map(h._1 -> (h._2 - howMany(h._1))))
      .filter(m => m._2 > 0)
  }

  def getBasketWithDiscountsApplied(basket: Basket,
                                    discounts: List[Discount]): Basket = {

    val discountedGoods = applyAllDiscounts(basket, discounts)

    val itemsNotDiscounted: Map[String, Int] =
      findNumberOfEachItemNotDiscounted(basket, discountedGoods)

    val goodsWithoutDiscountsApplied = (for {
      itemNotDiscounted <- itemsNotDiscounted
      priceGood = basket.goods
        .find(item => item.name == itemNotDiscounted._1)
        .get
        .price
      _ <- 0 until itemNotDiscounted._2
    } yield Good(itemNotDiscounted._1, priceGood, priceGood)).toList

    discountedGoods.add(Basket(goodsWithoutDiscountsApplied))
  }

}
