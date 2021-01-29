package com.nathangrand.adventofcode

import scala.io.Source

object Day2 extends App {

  case class IncorrectPasswordPolicy(letter: Char, minimumOccurrences: Int, maximumOccurrences: Int)

  case class CorrectPasswordPolicy(letter: Char, firstPosition: Int, secondPosition: Int)

  val parseRegex = """(\d+)-(\d+) ([a-z]): ([a-z]+)""".r

  def parseLineIncorrectPolicy(line: String): (IncorrectPasswordPolicy, String) = {
    val matchResult = parseRegex.findFirstMatchIn(line).get

    val minimumOccurrences = matchResult.group(1).toInt
    val maximumOccurrences = matchResult.group(2).toInt
    val letter = matchResult.group(3).head
    val password = matchResult.group(4)

    val policy = IncorrectPasswordPolicy(
      letter = letter, minimumOccurrences = minimumOccurrences, maximumOccurrences = maximumOccurrences
    )

    (policy, password)
  }

  def parseLineCorrectPolicy(line: String): (CorrectPasswordPolicy, String) = {
    val matchResult = parseRegex.findFirstMatchIn(line).get

    val firstPosition = matchResult.group(1).toInt
    val secondPosition = matchResult.group(2).toInt
    val letter = matchResult.group(3).head
    val password = matchResult.group(4)

    val policy = CorrectPasswordPolicy(
      letter = letter, firstPosition = firstPosition, secondPosition = secondPosition
    )

    (policy, password)
  }

  def passwordSatisfiesIncorrectPolicy(policy: IncorrectPasswordPolicy, password: String): Boolean = {
    val occurrencesOfLetter = password.count(_ == policy.letter)
    occurrencesOfLetter >= policy.minimumOccurrences && occurrencesOfLetter <= policy.maximumOccurrences
  }

  def passwordSatisfiesCorrectPolicy(policy: CorrectPasswordPolicy, password: String): Boolean = {

    def safeGet[T](seq: Seq[T], idx: Int): Option[T] = {
      if (seq.isDefinedAt(idx)) Some(seq(idx)); else None
    }

    val charAtFirstPosition = safeGet(password, policy.firstPosition - 1)
    val charAtSecondPosition = safeGet(password, policy.secondPosition - 1)

    val firstPositionValid = charAtFirstPosition.contains(policy.letter)
    val secondPositionValid = charAtSecondPosition.contains(policy.letter)

    Seq(firstPositionValid, secondPositionValid).count(identity) == 1
  }

  val policiesAndPasswordsPath = "C:/Dev/AdventOfCode/Day2Input.txt"
  val rawLines = Source.fromFile(policiesAndPasswordsPath).getLines.toList

  val incorrectPoliciesAndPasswords = rawLines.map(parseLineIncorrectPolicy)
  val validPasswordsIncorrectPolicyCount = incorrectPoliciesAndPasswords.count {
    case (policy, password) => passwordSatisfiesIncorrectPolicy(policy, password)
  }

  val correctPoliciesAndPasswords = rawLines.map(parseLineCorrectPolicy)
  val validPasswordsCorrectPolicyCount = correctPoliciesAndPasswords.count {
    case (policy, password) => passwordSatisfiesCorrectPolicy(policy, password)
  }

  println(validPasswordsIncorrectPolicyCount)
  println(validPasswordsCorrectPolicyCount)

}