package tech.houssemnasri
package day08

import java.util
import java.util.Scanner
import scala.::
import scala.collection.mutable

@main
def main(): Unit = {

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

  def partTwo(input: List[String]): Int = {
    val gridSize = input.size

    val grid = createGridFromInput(input)

    val (leftToRightMaxLookup, rightToLeftMaxLookup, topToBottomMaxLookup, bottomToTopMaxLookup) = createMaxLookupTables(grid)
    val numberOfSmallerTreesOnLeftLookup = Array.ofDim[Int](gridSize, gridSize)
    val numberOfSmallerTreesOnRightLookup = Array.ofDim[Int](gridSize, gridSize)
    val numberOfSmallerTreesOnTopLookup = Array.ofDim[Int](gridSize, gridSize)
    val numberOfSmallerTreesOnBottomLookup = Array.ofDim[Int](gridSize, gridSize)

    for (row <- 0 until gridSize) {
      numberOfSmallerTreesOnLeftLookup(row)(0) = 0
      for (i <- 1 until gridSize) {
        val element = grid(row)(i)
        val prevElement = grid(row)(i - 1)
        if (element > prevElement) {
          if (element >= leftToRightMaxLookup(row)(i)) {
            numberOfSmallerTreesOnLeftLookup(row)(i) = i
          } else {
            numberOfSmallerTreesOnLeftLookup(row)(i) = numberOfSmallerTreesOnLeftLookup(row)(i - 1) + 1
          }
        } else if (element <= prevElement) {
          numberOfSmallerTreesOnLeftLookup(row)(i) = 1
        }
      }
      // println("Waa " + numberOfSmallerTreesOnLeftLookup(row).mkString(", "))
    }

    println()

    for (row <- 0 until gridSize) {
      numberOfSmallerTreesOnRightLookup(row)(gridSize - 1) = 0
      var wall = -1
      var wallIndex = gridSize - 1
      for (i <- gridSize - 2 to 0 by -1) {
        val element = grid(row)(i)
        val prevElement = grid(row)(i + 1)
        if (element > prevElement) {
          if (element >= wall) {
            numberOfSmallerTreesOnRightLookup(row)(i) = wallIndex - i + (if (wallIndex == gridSize - 1) 0 else 1)
          } else {
            numberOfSmallerTreesOnRightLookup(row)(i) = numberOfSmallerTreesOnRightLookup(row)(i + 1) + 1
          }
        } else if (element <= prevElement) {
          wall = prevElement
          wallIndex = i + 1
          numberOfSmallerTreesOnRightLookup(row)(i) = 1
        }
      }
      // println("KAA " + numberOfSmallerTreesOnRightLookup(row).mkString(", "))
    }

    for (column <- 0 until gridSize) {
      numberOfSmallerTreesOnTopLookup(column)(0) = 0
      for (i <- 1 until gridSize) {
        val element = grid(i)(column)
        val prevElement = grid(i - 1)(column)
        if (element > prevElement) {
          if (element >= topToBottomMaxLookup(column)(i)) {
            numberOfSmallerTreesOnTopLookup(column)(i) = i
          } else {
            numberOfSmallerTreesOnTopLookup(column)(i) = numberOfSmallerTreesOnTopLookup(column)(i - 1) + 1
          }
        } else if (element <= prevElement) {
          numberOfSmallerTreesOnTopLookup(column)(i) = 1
        }
      }
      println("NNA " + numberOfSmallerTreesOnTopLookup(column).mkString(", "))
    }

    for (column <- 0 until gridSize) {
      numberOfSmallerTreesOnBottomLookup(column)(gridSize - 1) = 0
      println(bottomToTopMaxLookup(column).mkString(", "))
      for (i <- gridSize - 2 to 0 by -1) {
        val element = grid(i)(column)
        val prevElement = grid(i + 1)(column)
        if (element > prevElement) {

          if (element >= bottomToTopMaxLookup(column)(i)) {
            println(gridSize - i - 1)
            numberOfSmallerTreesOnBottomLookup(column)(i) = gridSize - i - 1
          } else {
            numberOfSmallerTreesOnBottomLookup(column)(i) = numberOfSmallerTreesOnBottomLookup(column)(i + 1) + 1
          }
        } else if (element <= prevElement) {
          numberOfSmallerTreesOnBottomLookup(column)(i) = 1
        }
      }
      println("KAWAAA " + numberOfSmallerTreesOnBottomLookup(column).mkString(", "))
    }

    val x = (0 until gridSize).flatMap(i => (0 until gridSize).map(j => (i, j)) ).map((i, j) => {
      val res = numberOfSmallerTreesOnLeftLookup(i)(j) *
        numberOfSmallerTreesOnRightLookup(i)(j) *
        numberOfSmallerTreesOnTopLookup(j)(i) *
        numberOfSmallerTreesOnBottomLookup(j)(i)
      println(f"($i, $j)" + " =>" + res)
        res
    }).max
    println(numberOfSmallerTreesOnLeftLookup(2)(3))
    println(numberOfSmallerTreesOnRightLookup(2)(3))
    println(numberOfSmallerTreesOnTopLookup(3)(2))
    println(numberOfSmallerTreesOnBottomLookup(3)(2))
    println(x)
    -1
  }

  val input = readInput(dayNumber = 8)
  println(partOne(input))
  println(partTwo(input))
}