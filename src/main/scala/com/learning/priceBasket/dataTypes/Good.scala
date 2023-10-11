package com.learning.priceBasket.dataTypes

case class Good(name: String, price: Double, discountedPrice: Double) {

  def getCost(discounted: Boolean): Double =
    if (discounted) this.discountedPrice
    else this.price
}

case class Basket(goods: List[Good]) {

  def countQuantityOfEach: Map[String, Int] =
    goods.map(g => g.name).groupBy(identity).mapValues(_.size).toMap

  def add(basket: Basket): Basket = Basket(this.goods ::: basket.goods)

  def equals(basket: Basket): Boolean = {
    this.goods.toSet == basket.goods.toSet && this.goods.size == basket.goods.size
  }
}

object Basket {

  def apply(items: List[String], pricesMap: Map[String, Double]): Basket =
    Basket(items.map(item => {
      if(pricesMap.contains(item)){
        Good(item, pricesMap(item), pricesMap(item))
      }else{
        println("item "+item +" not available")
        null
      }
    } )filter(_ != null))
}
