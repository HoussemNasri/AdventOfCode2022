package tech.houssemnasri

import java.io.File
import java.nio.file.Files
import scala.io.Source

/**
 * Reads and Returns the lines of advent of code input for the given day
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
