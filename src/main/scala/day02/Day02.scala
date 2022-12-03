package tech.houssemnasri
package day02

@main
def main(): Unit = {

  val rockPaperScissorsScoreLookup = Map(
    ('A', 'X') -> 3,
    ('A', 'Y') -> 6,
    ('A', 'Z') -> 0,
    ('B', 'X') -> 0,
    ('B', 'Y') -> 3,
    ('B', 'Z') -> 6,
    ('C', 'X') -> 6,
    ('C', 'Y') -> 0,
    ('C', 'Z') -> 3)

  def extractRoundInput(roundLine: String): (Char, Char) = {
    val roundElements = roundLine.split(' ').map(_.charAt(0))
    (roundElements(0), roundElements(1))
  }

  def partOne(input: List[String]): Int = {
    val roundsInput = input.map(extractRoundInput)
    var totalScore = 0
    for (opponent: Char, player: Char) <- roundsInput do {
      totalScore += player - 'X' + 1
      totalScore += rockPaperScissorsScoreLookup((opponent, player))
    }
    totalScore
  }

  def partTwo(input: List[String]): Int = {
    val roundsInput = input.map(extractRoundInput)
    var totalScore = 0
    for (opponent: Char, expectedResult: Char) <- roundsInput do {
      val expectedPlayerScore = if (expectedResult == 'X') 0 else if (expectedResult == 'Y') 3 else 6
      val player = rockPaperScissorsScoreLookup.find(p => p._1._1 == opponent && p._2 == expectedPlayerScore).get._1._2
      totalScore += player - 'X' + 1
      totalScore += rockPaperScissorsScoreLookup((opponent, player))
    }
    totalScore
  }

  val input = readInput(2)
  println(partOne(input))
  println(partTwo(input))
}