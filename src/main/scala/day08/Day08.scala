package tech.houssemnasri
package day08

import java.util
import java.util.Scanner
import scala.::
import scala.collection.mutable

@main
def main(): Unit = {

  def indices(grid: Array[Array[Int]]): Seq[(Int, Int)] =
    for {
      row <- grid.indices
      column <- grid.head.indices
    } yield (row, column)

  def createMaxLookupTables(grid: Array[Array[Int]]) = {
    val gridSize = grid.length

    val leftMaxLookupTable = Array.ofDim[Int](gridSize, gridSize)
    val rightMaxLookupTable = Array.ofDim[Int](gridSize, gridSize)
    val topMaxLookupTable = Array.ofDim[Int](gridSize, gridSize)
    val bottomMaxLookupTable = Array.ofDim[Int](gridSize, gridSize)

    for (i <- 0 until gridSize) {
      var maxLeft = -1
      var maxRight = -1
      var maxTop = -1
      var maxBottom = -1
      for (j <- 0 until gridSize) {
        val left = grid(i)(j);
        val right = grid(i)(gridSize - j - 1)
        val top = grid(j)(i)
        val bottom = grid(gridSize - j - 1)(i)

        val prevMaxLeft = maxLeft
        val prevMaxRight = maxRight
        val prevMaxTop = maxTop
        val prevMaxBottom = maxBottom

        maxLeft = Math.max(maxLeft, left)
        maxRight = Math.max(maxRight, right)
        maxTop = Math.max(maxTop, top)
        maxBottom = Math.max(maxBottom, bottom)

        leftMaxLookupTable(i)(j) = if (left == prevMaxLeft) maxLeft + 1 else maxLeft
        rightMaxLookupTable(i)(gridSize - j - 1) = if (right == prevMaxRight) maxRight + 1 else maxRight
        topMaxLookupTable(i)(j) = if (top == prevMaxTop) maxTop + 1 else maxTop
        bottomMaxLookupTable(i)(gridSize - j - 1) = if (bottom == prevMaxBottom) maxBottom + 1 else maxBottom

      }
    }
    (leftMaxLookupTable, rightMaxLookupTable, topMaxLookupTable, bottomMaxLookupTable)
  }

  def createGridFromInput(input: List[String]): Array[Array[Int]] = {
    val gridSize = input.size
    val grid = Array.ofDim[Int](gridSize, gridSize)
    for ((line, rowIndex) <- input.zipWithIndex) {
      for ((char, colIndex) <- line.zipWithIndex) {
        grid(rowIndex)(colIndex) = char.asDigit
      }
    }
    grid
  }

  def partOne(input: List[String]): Int = {
    val gridSize = input.size

    val grid = createGridFromInput(input)

    val (leftToRightMaxLookup, rightToLeftMaxLookup, topToBottomMaxLookup, bottomToTopMaxLookup) = createMaxLookupTables(grid)

    var result = 0
    for (rowIndex <- 0 until gridSize) {
      for (colIndex <- 0 until gridSize) {
        val element = grid(rowIndex)(colIndex)
        val canBeSeenFromLeft = leftToRightMaxLookup(rowIndex)(colIndex) == element
        val canBeSeenFromRight = rightToLeftMaxLookup(rowIndex)(colIndex) == element
        val canBeSeenFromTop = topToBottomMaxLookup(colIndex)(rowIndex) == element
        val canBeSeenFromBottom = bottomToTopMaxLookup(colIndex)(rowIndex) == element

        result = if (canBeSeenFromLeft || canBeSeenFromRight || canBeSeenFromTop || canBeSeenFromBottom) result + 1 else result
      }
    }

    result
  }

  // Took from https://github.com/AvaPL/Advent-of-Code-2022/tree/main/src/main/scala/day8
  def partTwo(input: List[String]): Int = {
    val grid = createGridFromInput(input)
    indices(grid).map { case (row, column) =>
      scenicScore(grid, row, column)
    }.max
  }

  def scenicScore(grid: Array[Array[Int]], row: Int, column: Int) =
    Seq(
      topScenicScore _,
      bottomScenicScore _,
      leftScenicScore _,
      rightScenicScore _
    )
      .map(_(grid, row, column))
      .product

  def topScenicScore(grid: Array[Array[Int]], row: Int, column: Int) = {
    var topScore = 0
    (0 until row).findLast { aboveIndex =>
      topScore += 1
      grid(aboveIndex)(column) >= grid(row)(column)
    }
    topScore
  }

  def bottomScenicScore(grid: Array[Array[Int]], row: Int, column: Int) = {
    var bottomScore = 0
    (row + 1 until grid.length).find { belowIndex =>
      bottomScore += 1
      grid(belowIndex)(column) >= grid(row)(column)
    }
    bottomScore
  }

  def leftScenicScore(grid: Array[Array[Int]], row: Int, column: Int) = {
    var leftScore = 0
    (0 until column).findLast { leftIndex =>
      leftScore += 1
      grid(row)(leftIndex) >= grid(row)(column)
    }
    leftScore
  }

  def rightScenicScore(grid: Array[Array[Int]], row: Int, column: Int) = {
    var rightScore = 0
    (column + 1 until grid.head.length).find { rightIndex =>
      rightScore += 1
      grid(row)(rightIndex) >= grid(row)(column)
    }
    rightScore
  }

  val input = readInput(dayNumber = 8)
  println(partOne(input))
  println(partTwo(input))
}