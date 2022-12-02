package tech.houssemnasri
package day01

@main
def main(): Unit = {
  def partOne(input: List[String]): Int = {
    sortedInventories(input).head
  }

  def partTwo(input: List[String]): Int = {
    sortedInventories(input).take(3).sum
  }

  def sortedInventories(input: List[String]): List[Int] = {
    var inventoryList = List[List[Int]]()
    var currentInventory = List[Int]()
    for line <- input.appended("") do {
      if (line.isEmpty) {
        inventoryList = inventoryList.appended(currentInventory)
        currentInventory = List()
      } else {
        currentInventory = currentInventory.appended(line.toInt)
      }
    }
    inventoryList.map(_.sum).sorted(using Ordering.Int.reverse)
  }

  val input = readInput(1)
  println(partOne(input))
  println(partTwo(input))
}