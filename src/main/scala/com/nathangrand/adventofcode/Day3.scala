package com.nathangrand.adventofcode

import scala.io.Source

object Day3 extends App {

  case class RepeatingStripMap(mapStrip: Array[Array[Char]]) {

    val height = mapStrip.size
    val stripWidth = mapStrip.head.size

    // Top left corner of map is (x = 0, y = 0)
    def objectAtPosition(x: Int, y: Int): Char = {
      if (y < 0 || y > height - 1) {
        throw new IllegalArgumentException(s"Y coordinate $y is invalid for map with $height rows that is zero indexed")
      } else {
        val row = mapStrip(y)
        row(x % stripWidth)
      }
    }

    def treeExistsAtPosition(x: Int, y: Int): Boolean = {
      objectAtPosition(x, y) == '#'
    }

  }

  def countTreesOnPath(map: RepeatingStripMap, xStep: Int, yStep: Int): Long = {
    val positions = (0 until map.height by yStep).zipWithIndex.map { case (y, idx) => (idx * xStep, y) }
    val treeCount = positions.count { case (x, y) => map.treeExistsAtPosition(x, y) }
    treeCount.toLong
  }

  val mapStripPath = "C:/Dev/AdventOfCode/Day3Input.txt"
  val rawLines = Source.fromFile(mapStripPath).getLines
  val mapStrip = rawLines.toArray.map(_.toArray)
  val map = RepeatingStripMap(mapStrip)

  val result_1_1 = countTreesOnPath(map, 1, 1)
  val result_3_1 = countTreesOnPath(map, 3, 1)
  val result_5_1 = countTreesOnPath(map, 5, 1)
  val result_7_1 = countTreesOnPath(map, 7, 1)
  val result_1_2 = countTreesOnPath(map, 1, 2)

  val part2Answer = result_1_1 * result_3_1 * result_5_1 * result_7_1 * result_1_2

  println(s"Part 1 answer: $result_3_1")
  println(s"Part 2 answer: $part2Answer")

}