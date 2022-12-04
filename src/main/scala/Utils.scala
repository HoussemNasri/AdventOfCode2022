package tech.houssemnasri

import java.io.File
import java.nio.file.Files
import scala.io.Source

/**
 * Reads the input file and returns the list of advent of code inputs for the given day
 * */
def readInput(dayNumber: Int): List[String] = {
  val inputFile = new File(s"./src/main/scala/day0$dayNumber/Day0$dayNumber.txt")
  if (!inputFile.exists()) {
    throw IllegalStateException(s"Couldn't find the input file at ${inputFile.getAbsoluteFile}")
  }

  val inputSource = Source.fromFile(inputFile)
  if (inputSource.isEmpty) {
    inputSource.close()
    throw new IllegalStateException(s"Input file of Day0$dayNumber is empty")
  }

  val inputLines = inputSource.getLines().toList
  inputSource.close()

  inputLines
}

def toTuple3[A](list: List[A]): (A, A, A) = {
  list match
    case item1 :: item2 :: item3 :: Nil => (item1, item2, item3)
    case _ => throw new IllegalStateException()
}

def toTuple2[A](list: Iterable[A]): (A, A) = {
  list match
    case item1 :: item2 :: Nil => (item1, item2)
    case _ => throw new IllegalStateException()
}
