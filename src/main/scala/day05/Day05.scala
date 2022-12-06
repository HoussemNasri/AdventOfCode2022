package tech.houssemnasri
package day05

import java.util
import java.util.Scanner
import scala.collection.mutable

@main
def main(): Unit = {
  case class MoveCommand(amount: Int, fromPos: Int, toPos: Int)
  enum CrateMoverVersion:
    case Version9000, Version9001

  def parseCratesInput(cratesInput: List[String]): IndexedSeq[mutable.Stack[Char]] = {
    val numberOfStacks = cratesInput.last.dropRight(1).last - '0'
    // Dropping the stack indices line and reversing the list to insert elements into stacks easily
    val cleanCratesInput = cratesInput.dropRight(1).reverse

    val crateStacks = for {_ <- 0 until numberOfStacks} yield new mutable.Stack[Char]()
    cleanCratesInput.zipWithIndex.foreach((line: String, i: Int) => {
      var readOffset = 0
      var stackIndex = 0
      while (readOffset < line.length) {
        val crate = line.substring(readOffset, Math.min(line.length, readOffset + 4))(1)
        if (crate.isLetter) crateStacks(stackIndex).push(crate)
        readOffset = readOffset + 4
        stackIndex += 1
      }
    })

    crateStacks
  }

  def parseCommandsInput(commandsInput: List[String]): List[MoveCommand] = {
    commandsInput.map(command => {
      val commandParts = command.split(' ')
      val amount = commandParts(1).toInt
      val fromPos = commandParts(3).toInt
      val toPos = commandParts(5).toInt

      MoveCommand(amount, fromPos, toPos)
    })
  }

  def partOne(input: List[String]): String = {
    val cratesAndCommandSeparatorPos = input.indexOf("")
    val (cratesInput, commandsInput) = input.splitAt(cratesAndCommandSeparatorPos)
    val crateStacks = parseCratesInput(cratesInput)
    val commandList = parseCommandsInput(commandsInput.tail)

    executeCommands(commandList, crateStacks)

    crateStacks.map(stack => stack.top).mkString
  }

  def executeCommands(commands: List[MoveCommand], crateStacks: IndexedSeq[mutable.Stack[Char]], crateMoverVersion: CrateMoverVersion = CrateMoverVersion.Version9000): Unit = {
    for (command <- commands) {
      val sourceStack = crateStacks(command.fromPos - 1)
      val destStack = crateStacks(command.toPos - 1)
      val poppedStack = mutable.Stack[Char]()
      (1 to command.amount).foreach(_ => {
        poppedStack.push(sourceStack.pop())
      })
      destStack.pushAll(if (crateMoverVersion == CrateMoverVersion.Version9001) poppedStack else poppedStack.reverse)
    }
  }

  def partTwo(input: List[String]): String = {
    val cratesAndCommandSeparatorPos = input.indexOf("")
    val (cratesInput, commandsInput) = input.splitAt(cratesAndCommandSeparatorPos)
    val crateStacks = parseCratesInput(cratesInput)
    val commandList = parseCommandsInput(commandsInput.tail)

    executeCommands(commandList, crateStacks, CrateMoverVersion.Version9001)

    crateStacks.map(stack => stack.top).mkString
  }

  val input = readInput(dayNumber = 5)
  println(partOne(input))
  println(partTwo(input))
}