package tech.houssemnasri
package day06

import java.util
import java.util.Scanner
import scala.collection.mutable

@main
def main(): Unit = {
  def partOne(input: String): Int = {
    for (i <- 0 until input.length - 3) {
      if (isSubstringHasDistinctCharacters(input, i until i + 4)) {
        return i + 4
      }
    }
    -1
  }

  def partTwo(input: String): Int = {
    for (i <- 0 until input.length - 13) {
      if (isSubstringHasDistinctCharacters(input, i until i + 14)) {
        return i + 14
      }
    }
    -1
  }

  def isSubstringHasDistinctCharacters(sourceText: String, substringRange: Range): Boolean = {
    val charsSeenLookup = new Array[Boolean](26)
    for (i <- substringRange) {
      val characterIndexInAlphabet = sourceText(i) - 'a'
      if (charsSeenLookup(characterIndexInAlphabet)) {
        return false
      } else {
        charsSeenLookup(characterIndexInAlphabet) = true
      }
    }
    true
  }

  val input = readInput(dayNumber = 6)
  println(partOne(input.head))
  println(partTwo(input.head))
}