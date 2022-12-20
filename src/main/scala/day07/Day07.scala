package tech.houssemnasri
package day07

import java.util
import java.util.Scanner
import scala.::
import scala.collection.mutable

@main
def main(): Unit = {
  trait AbstractFile:
    def dump(level: Int = 0): Unit
    def getSize: Int

  case class Directory(name: String, parent: Directory = null) extends AbstractFile {
    private var filesAndDirectoriesInside = List[AbstractFile]()

    def getContent: List[AbstractFile] = {
      filesAndDirectoriesInside
    }

    def addFileIfNotExists(file: AbstractFile): Unit = {
      if (!filesAndDirectoriesInside.contains(file)) {
        filesAndDirectoriesInside = filesAndDirectoriesInside.appended(file)
      }
    }

    def navigateTo(directory: String): Directory = {
      filesAndDirectoriesInside.find(f => f match
        case Directory(name, _) => name == directory
        case _ => false)
    }.map(_.asInstanceOf[Directory]).get

    def up(): Directory = {
      if (parent == null) {
        this
      } else {
        parent
      }
    }

    override def getSize: Int = {
      filesAndDirectoriesInside.map(_.getSize).sum
    }

    override def dump(level: Int = 0): Unit = {
      println(s"- ${name} (dir)")
      filesAndDirectoriesInside.foreach(f => {
        print("   ".repeat(level + 1))
        f.dump(level + 1)
      })
    }
  }

  case class File(fileSize: Int, name: String) extends AbstractFile {
    override def getSize: Int = {
      fileSize
    }

    override def dump(level: Int = 0): Unit = {
      println(s"- $name (file, $fileSize)")
    }
  }

  def findAllDirectoriesSizes(root: Directory): List[Int] = {
    root.getSize :: root.getContent.flatMap(f => f match
      case d: Directory => findAllDirectoriesSizes(d)
      case _ => List.empty)
  }

  def partOne(input: List[String]): Int = {
    val root = parseFileTree(input)
    findAllDirectoriesSizes(root).filter(_<=100_000).sum
  }

  def partTwo(input: List[String]): Int = {
    val root = parseFileTree(input)
    val toFreeUpSpace = root.getSize - 40_000_000
    findAllDirectoriesSizes(root).filter(_>= toFreeUpSpace).min
  }

  def parseFileTree(input: List[String]): Directory = {
    val root = Directory("/")
    var currentDirectory = root
    var lst = input.tail

    while (lst.nonEmpty) {
      val command = lst.head
      val parts = command.split(' ').tail
      if (parts.head == "cd") {
        currentDirectory = cd(currentDirectory, parts.tail.head)
        lst = lst.tail
      } else {
        lst = ls(currentDirectory, lst.tail)
      }
    }
    root
  }

  def cd(currDirectory: Directory, destination: String): Directory = {
    if (destination == "..") {
      currDirectory.up()
    } else {
      val destDirectory = Directory(destination, currDirectory)
      currDirectory.addFileIfNotExists(destDirectory)
      currDirectory.navigateTo(destination)
    }
  }

  def ls(currDirectory: Directory, input: List[String]): List[String] = {
    var lst = input
    while (lst.nonEmpty && !lst.head.startsWith("$")) {
      currDirectory.addFileIfNotExists(parseAbstractFile(lst.head, currDirectory))
      lst = lst.tail
    }
    lst
  }

  def parseAbstractFile(str: String, currentDirectory: Directory): AbstractFile = {
    val parts = str.split(' ')
    if (parts.head == "dir") {
      Directory(parts.tail.head, currentDirectory)
    } else {
      File(parts.head.toInt, parts.tail.head)
    }
  }

  val input = readInput(dayNumber = 7)
  println(partOne(input))
  println(partTwo(input))
}