package com.learning.priceBasket.utils

import com.learning.priceBasket.dataTypes.Discount.{
  Condition,
  ConditionalDiscount,
  Discount
}
import scala.io.Source

object ReadFile {

  def readCSVToMap(path: String): Map[String, Double] = {
    val file =
      Source
        .fromFile(path)

    file.getLines
      .map(line => {
        val cols = line.split(",").map(_.trim)
        Map(cols(0) -> cols(1).toDouble)
      })
      .toList
      .reduce(_ ++ _)
  }

  def readCSVToDiscount(path: String,
                        countOfItems: Map[String, Int]): List[Discount] = {
    val file =
      Source
        .fromFile(path)

    file.getLines
      .map(line => {
        val cols: Array[String] = line.split(",").map(_.trim)
        if(countOfItems.contains(cols(0))) {
          Discount(cols(0), cols(1).toDouble, countOfItems(cols(0)))
        }else{
          null
        }
      }).filter(_ != null)
      .toList
  }

  def readCSVToConditionalDiscount(path: String): List[ConditionalDiscount] = {
    val file =
      Source
        .fromFile(path)

    file.getLines
      .map(line => {
        val cols: Array[String] = line.split(",").map(_.trim)
        ConditionalDiscount(
          cols(0),
          cols(1).toDouble,
          Condition(cols(2).split("/").map(_.trim).toList)
        )
      })
      .toList
  }
}
