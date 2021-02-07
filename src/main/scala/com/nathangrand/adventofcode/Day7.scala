package com.nathangrand.adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {

  val rulesPath = "C:/Dev/AdventOfCode/Day7Input.txt"
  val rulesAsStrings = Source.fromFile(rulesPath).getLines.toList
  val parsedRules = rulesAsStrings.map(parseRule).toMap

  val shinyGoldName = "shiny gold"

  val partOneAnswer = bagsThatCanContainBag(shinyGoldName, parsedRules).size
  println(partOneAnswer)

  val partTwoAnswer = countBagsInsideBag(shinyGoldName, parsedRules)
  println(partTwoAnswer)

  private def bagsThatCanContainBag(bag: String, rules: Map[String, Map[String, Int]]): Set[String] = {

    @tailrec
    def recurse(allBagsFoundSoFar: Set[String], newBagsFoundPreviousIteration: Set[String]): Set[String] = {
      val potentiallyNewBags = newBagsFoundPreviousIteration.flatMap(bagsThatCanDirectlyHoldBag)
      val newBagsThisIteration = potentiallyNewBags diff allBagsFoundSoFar
      val updatedAllBagsFoundSoFar = allBagsFoundSoFar ++ newBagsThisIteration
      if (newBagsThisIteration.isEmpty) {
        updatedAllBagsFoundSoFar
      } else {
        recurse(updatedAllBagsFoundSoFar, newBagsThisIteration)
      }
    }

    def bagsThatCanDirectlyHoldBag(bag: String): Set[String] = {
      rules.filter { case (_, v) => v.keySet.contains(bag) }.keySet
    }

    recurse(bagsThatCanDirectlyHoldBag(bag), bagsThatCanDirectlyHoldBag(bag))

  }

  private def countBagsInsideBag(bag: String, rules: Map[String, Map[String, Int]]): Int = {

    def getNestedBagCount(ruleForBag: Map[String, Int]): Int = {
      if (ruleForBag.isEmpty) {
        0
      } else {
        ruleForBag.map {
          case (bagType, numberOfBagsOfType) =>
            numberOfBagsOfType + (numberOfBagsOfType * getNestedBagCount(rules(bagType)))
        }.sum
      }
    }

    getNestedBagCount(rules(bag))

  }

  private def parseRule(rule: String): (String, Map[String, Int]) = {

    val initialSplitRegex = """^(.+) bags contain (.+).$""".r
    val containedBagRegex = """^(\d+) (.+)$""".r

    val initialMatch = initialSplitRegex.findFirstMatchIn(rule).get
    val focalBag = initialMatch.group(1).trim
    val containedBagsString = initialMatch.group(2).trim

    val containedBags = containedBagsString
      .replaceAll("bags", "")
      .replaceAll("bag", "")
      .replaceAll(" , ", ",")
      .split(",")
      .toList
      .flatMap {
        s => {
          val containedBagMatch = containedBagRegex.findFirstMatchIn(s)
          containedBagMatch.map(m => m.group(2).trim -> m.group(1).toInt)
        }
      }.toMap

    focalBag -> containedBags
  }

}
