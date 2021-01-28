package com.nathangrand.adventofcode

import scala.io.Source

object Day1 extends App {

  val expenseReportPath = "C:/Dev/AdventOfCode/Day1Input.txt"

  val amounts = Source.fromFile(expenseReportPath).getLines.map(_.toInt).toList

  val answers = {
    for {
      amount1 <- amounts
      amount2 <- amounts
      if amount1 + amount2 == 2020
    } yield amount1 * amount2
  }.distinct

  answers.foreach(println)

}
