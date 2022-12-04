package tech.houssemnasri
package day04

@main
def main(): Unit = {
  def isRangeContainedInOther(range: Range, other: Range) = {
    range.containsSlice(other)
  }

  def isRangeOverlapWithOther(range: Range, other: Range) = {
    range.start <= other.end && other.start <= range.end
  }

  def parseElfPairAssignedSections(elfPairRanges: String): (Range, Range) = {
    val listOfBothElfRanges = elfPairRanges.split(',').map(a => toTuple2(a.split('-').toList))
      .map((min: String, max: String) => min.toInt to max.toInt).toList
    assert(listOfBothElfRanges.length == 2)

    toTuple2(listOfBothElfRanges)
  }

  def partOne(input: List[String]): Int = {
    input.map(parseElfPairAssignedSections)
      .count((fElfRange, sElfRange) => isRangeContainedInOther(fElfRange, sElfRange) || isRangeContainedInOther(sElfRange, fElfRange))
  }

  def partTwo(input: List[String]): Int = {
    input.map(parseElfPairAssignedSections)
      .count((fElfRange, sElfRange) => isRangeOverlapWithOther(fElfRange, sElfRange))
  }

  val input = readInput(dayNumber = 4)
  println(partOne(input))
  println(partTwo(input))
}