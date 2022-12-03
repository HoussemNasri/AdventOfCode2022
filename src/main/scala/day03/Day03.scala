package tech.houssemnasri
package day03

@main
def main(): Unit = {
  def computeItemPriority(itemType: Char) : Int = {
    if (itemType.isLower) {
      itemType - 'a' + 1
    } else {
      itemType - 'A' + 27
    }
  }

  def partOne(input: List[String]): Int = {
    input.map(rucksack => rucksack.splitAt(rucksack.length / 2))
      .map((fHalf: String, sHalf: String) => fHalf.intersect(sHalf))
      .map(str => computeItemPriority(str.charAt(0))).sum
  }

  def partTwo(input: List[String]): Int = {
    input.grouped(3).map(elfGroup => {
      elfGroup match
        case fRucksack :: sRuckSack :: tRuckSack :: Nil => fRucksack.intersect(sRuckSack).intersect(tRuckSack)
        case _ => throw new IllegalStateException()
    }).map(str => computeItemPriority(str.charAt(0))).sum
  }

  val input = readInput(dayNumber = 3)
  println(partOne(input))
  println(partTwo(input))
}