package com.nathangrand.adventofcode

import scala.io.Source

object Day6 extends App {

  val customsDeclarationsPath = "C:/Dev/AdventOfCode/Day6Input.txt"
  val customsDeclarations =
    Source.fromFile(customsDeclarationsPath).mkString
      .split("\n\n").toList
      .map(_.split("\n").map(_.toCharArray.toSet).toList)

  val part1Answer = customsDeclarations.map(_.flatten.toSet.size).sum
  println(part1Answer)

  val part2Answer = customsDeclarations.map(group => group.reduce(_ intersect _).size).sum
  println(part2Answer)

}
