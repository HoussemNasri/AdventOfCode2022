package tech.houssemnasri
package day09

import java.util
import java.util.Scanner
import scala.::
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

enum Direction(val x: Int, val y: Int):
  object A {
    def parse(c: Char): Direction = {
      c match
        case 'L' => Direction.L
        case 'R' => Direction.R
        case 'U' => Direction.U
        case 'D' => Direction.D
    }
  }

  case L extends Direction(-1, 0)
  case R extends Direction(1, 0)
  case U extends Direction(0, 1)
  case D extends Direction(0, -1)

case class Motion(direction: Direction, amount: Int)

@main
def main(): Unit = {

  def parseMotions(input: List[String]): List[Motion] = {
    input.map(line => {
      val parts = line.split(' ')
      Motion(Direction.valueOf(parts.head), parts.tail.head.toInt)
    })
  }

  def partOne(input: List[String]): Int = {
    val motions = parseMotions(input)

    var headX = 0
    var headY = 0
    var tailX = 0
    var tailY = 0
    var prevHeadX = 0
    var prevHeadY = 0
    {
      for motion <- motions
          _ <- 1 to motion.amount yield {
        headX += motion.direction.x
        headY += motion.direction.y
        val isHeadTouchingTail = (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (dx, dy))).exists((dx, dy) => (headX + dx, headY + dy) == (tailX, tailY))
        if (!isHeadTouchingTail) {
          tailX = prevHeadX
          tailY = prevHeadY
        }
        prevHeadX = headX
        prevHeadY = headY
        (tailX, tailY)
      }
    }.distinct.size
  }


  def partTwo(input: List[String]): Int = {
    val motions = parseMotions(input)

    val rope: ListBuffer[(Int, Int)] = mutable.ListBuffer.fill(10)((0, 0))
    {
      for motion <- motions
          _ <- 1 to motion.amount yield {
        val (ropeHeadX, ropeHeadY) = rope.head
        rope(0) = (ropeHeadX + motion.direction.x, ropeHeadY + motion.direction.y)
        for (knotIndex <- 1 until rope.size) {
          val (headKnotX, headKnotY) = rope(knotIndex - 1)
          var (tailKnotX, tailKnotY) = rope(knotIndex)

          val shouldMoveTail = Math.abs(headKnotX - tailKnotX) > 1 || Math.abs(headKnotY - tailKnotY) > 1
          if (shouldMoveTail) {
            val headAndTailOnTheSameRow = headKnotY == tailKnotY
            val headAndTailOnTheSameColumn = headKnotX == tailKnotX

            if (headAndTailOnTheSameRow) {
              val tailBehindHead = tailKnotX < headKnotX
              if (tailBehindHead) {
                tailKnotX = headKnotX - 1
              } else {
                tailKnotX = headKnotX + 1
              }
            } else if (headAndTailOnTheSameColumn) {
              val tailBehindHead = tailKnotY < headKnotY
              if (tailBehindHead) {
                tailKnotY = headKnotY - 1
              } else {
                tailKnotY = headKnotY + 1
              }
            } else {
              val closeDiagonal: (Int, Int) = List((-1, -1), (-1, 1), (1, -1), (1, 1)).find((x, y) => {
                Math.abs(headKnotX - (tailKnotX + x)) <= 1 && Math.abs(headKnotY - (tailKnotY + y)) <= 1
              }).get

              tailKnotX = tailKnotX + closeDiagonal._1
              tailKnotY = tailKnotY + closeDiagonal._2
            }
          }
          rope(knotIndex) = (tailKnotX, tailKnotY)
        }
        rope.last
      }
    }.distinct.size
  }

  val input = readInput(dayNumber = 9)
  println(partOne(input))
  println(partTwo(input))
}