package com.nathangrand.adventofcode

import scala.io.Source

object Day5 extends App {

  case class BoardingPass(seat: String) {

    def getRowNumber: Int = {

      case class RowRange(lower: Int, upper: Int)

      val rowPartOfBoardingPass = seat.substring(0, 7)

      val rowRange = rowPartOfBoardingPass.foldLeft(RowRange(0, 127)) {
        case (range, character) =>
          val diff = range.upper - range.lower
          val updatedRange = if (character == 'F') {
            RowRange(range.lower, range.lower + diff / 2)
          } else {
            RowRange(range.upper - diff / 2, range.upper)
          }
          updatedRange
      }

      assert(rowRange.lower == rowRange.upper, s"Row lower: ${rowRange.lower} did not equal upper: ${rowRange.upper}")

      rowRange.lower
    }

    def getColumn: Int = {
      val columnPartOfBoardingPass = seat.substring(7, 10)
      columnPartOfBoardingPass match {
        case "LLL" => 0
        case "LLR" => 1
        case "LRL" => 2
        case "LRR" => 3
        case "RLL" => 4
        case "RLR" => 5
        case "RRL" => 6
        case "RRR" => 7
      }
    }

    def getSeatNumber: Int = getRowNumber * 8 + getColumn

  }

  val boardingPassesPath = "C:/Dev/AdventOfCode/Day5Input.txt"
  val boardingPasses = Source.fromFile(boardingPassesPath).getLines.toList.map(BoardingPass)

  val partOneAnswer = boardingPasses.map(_.getSeatNumber).max
  println(partOneAnswer)

  val totalSeats = 128 * 8
  val allSeats = (0 to totalSeats).toSet
  val boardingPassSeats = boardingPasses.map(_.getSeatNumber).toSet
  val seatsWithoutBoardingPass = allSeats.diff(boardingPassSeats).toSeq.sorted

  val part2Answer = {
    val meetingCriteria = seatsWithoutBoardingPass.sliding(3).toSeq.filter {
      r => (r(1) - r(0) != 1) && (r(2) - r(1) != 1)
    }
    assert(meetingCriteria.size == 1, s"Found more than one possible seat")
    val answer = meetingCriteria.head(1)
    answer
  }
  println(part2Answer)

}
