package com.nathangrand.adventofcode

import scala.io.Source
import scala.util.matching.Regex

object Day4 extends App {

  val passportPath = "C:/Dev/AdventOfCode/Day4Input.txt"
  val rawFileAsString = Source.fromFile(passportPath).mkString
  val passportStrings = rawFileAsString.split("\n\n").map(_.replaceAll("\n", " "))
  val passportFields = passportStrings
    .map(_.split(" "))
    .map(_.map(_.split(":")).map(r => (r(0), r(1))).toMap)

  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  val validPassportCountByInitialRules = passportFields.count(validByInitialRules)
  val validPassportCountByUpdatedRules = passportFields.count(validByUpdatedRules)

  println(validPassportCountByInitialRules)
  println(validPassportCountByUpdatedRules)

  def regexMatches(r: Regex, s: String): Boolean = {
    r.findFirstMatchIn(s).isDefined
  }

  def validByr(s: String): Boolean = {
    regexMatches("""^\d{4}$""".r, s) && s.toInt >= 1920 && s.toInt <= 2002
  }

  def validIyr(s: String): Boolean = {
    regexMatches("""^\d{4}$""".r, s) && s.toInt >= 2010 && s.toInt <= 2020
  }

  def validEyr(s: String): Boolean = {
    regexMatches("""^\d{4}$""".r, s) && s.toInt >= 2020 && s.toInt <= 2030
  }

  def validHgt(s: String): Boolean = {
    def validCm(s: String): Boolean = {
      regexMatches("""^\d{3}cm$""".r, s) && s.substring(0, 3).toInt >= 150 && s.substring(0, 3).toInt <= 193
    }

    def validIn(s: String): Boolean = {
      regexMatches("""^\d{2}in$""".r, s) && s.substring(0, 2).toInt >= 59 && s.substring(0, 2).toInt <= 76
    }

    validCm(s) || validIn(s)
  }

  def validHcl(s: String): Boolean = {
    regexMatches("""^#[0-9a-f]{6}$""".r, s)
  }

  def validEcl(s: String): Boolean = {
    val validValues = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    validValues.contains(s)
  }

  def validPid(s: String): Boolean = {
    regexMatches("""^[0-9]{9}$""".r, s)
  }

  def validByInitialRules(fields: Map[String, String]): Boolean = {
    (requiredFields diff fields.keySet).isEmpty
  }

  def validByUpdatedRules(fields: Map[String, String]): Boolean = {
    validByInitialRules(fields) &&
      validByr(fields("byr")) &&
      validIyr(fields("iyr")) &&
      validEyr(fields("eyr")) &&
      validHgt(fields("hgt")) &&
      validHcl(fields("hcl")) &&
      validEcl(fields("ecl")) &&
      validPid(fields("pid"))
  }

}