import scala.collection.immutable.Queue
import scala.collection.immutable.Map
import scala.collection.immutable.ListMap
import sun.misc.Signal

object Main {
  val SIZE_OF_DISPLAYED_MAP: Int = 10
  val SIZE_OF_WORDS: Int = 6
  val SIZE_OF_WINDOW: Int = 1000
  val BAD_WORDS: Array[String] = Array.empty[String]

  def tablePrint(freqTable: ListMap[String, Int]): Unit = {
    println("")
    println("Words by order of frequency")
    freqTable.foreach { println }
    println("")
  }

  def main(args: Array[String]): Unit = {
    var queueSize: Int = SIZE_OF_WINDOW
    var wordSize: Int = SIZE_OF_WORDS
    var mapSize: Int = SIZE_OF_DISPLAYED_MAP
    var ignoreWords = BAD_WORDS
    if (args.length == 1) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt
    } else if (args.length == 2) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt

      if (args(1).toInt < 0) {
        throw new NumberFormatException("Please give a please give a number greater than 0 for arg 2");
      }
      wordSize = args(1).toInt
    } else if (args.length == 3) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt

      if (args(1).toInt < 0) {
        throw new NumberFormatException("Please give a non-negative natural number for arg 2");
      }
      wordSize = args(1).toInt

      if (args(2).toInt > args(0).toInt) {
        throw new NumberFormatException("Value for arg 3 cannot be larger than value for arg 1");
      }
      queueSize = args(2).toInt
    } else if (args.length == 4) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt

      if (args(1).toInt < 0) {
        throw new NumberFormatException("Please give a non-negative natural number for arg 2");
      }
      wordSize = args(1).toInt

      if (args(2).toInt > args(0).toInt) {
        throw new NumberFormatException("Value for arg 3 cannot be larger than value for arg 1");
      }
      queueSize = args(2).toInt

      ignoreWords = args(3).split(" ")
    }

    val iterator = scala.io.Source.stdin.getLines
    val words = iterator.flatMap(_.split("(?U)[^\\p{Alpha}0-9']+"))
    val window: inputHandler = new inputHandler(queueSize, wordSize, ignoreWords, mapSize)
    val a = window.checkStuff(words)
    a.foreach { tablePrint }

  }

}

class inputHandler(queueSize: Int, wordSize: Int, wordCheck: Array[String], mapSize: Int) {
  //These are the scanLeft operations we need to use
  var memCounter = 0
  val queueAdding = (x: Queue[String], y: String) => {
    val inputQ = x
    val inputStr = y
    if (checkAcceptableWord(inputStr, wordSize, wordCheck) == 0) {
      val newInputQ = inputQ.enqueue(inputStr.toLowerCase())
      if (newInputQ.length > queueSize) {
        val dumped = newInputQ.dequeue
        val queue = dumped._2
        queue
      } else {
        val queue = newInputQ
        queue
      }
    } else {
      val queue = x
      queue
    }
  }

  def checkStuff(input: Iterator[String]): Iterator[ListMap[String, Int]] = {
    val runningTotal = input.scanLeft(Queue.empty[String])(queueAdding)
    runningTotal.map { groupUp }

  }
  def groupUp(input: Queue[String]): ListMap[String, Int] = {
    val freq = input.groupBy(identity).mapValues(_.size)
    val sortedFreq = ListMap(freq.toSeq.sortWith(_._2 < _._2): _*)
    val sizeFreq = sortedFreq.drop(sortedFreq.size - mapSize)
    val sizeFreqTwo = sortedFreq.drop(sortedFreq.size - mapSize)
    memCounter = memCounter + 1
    if (memCounter > 100) {
      val mb = 1024 * 1024
      val runtime = Runtime.getRuntime
      println("")
      Console.err.print("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
      Console.err.print(", " + "** Free Memory:  " + runtime.freeMemory / mb)
      Console.err.print(", " + "** Total Memory: " + runtime.totalMemory / mb)
      Console.err.print(", " + "** Max Memory:   " + runtime.maxMemory / mb)
      println("")
      println("")
      memCounter = 0
    }
    sizeFreqTwo

  }

  def checkAcceptableWord(inputWord: String, minLength: Int, ignoreWords: Array[String]): Int = {
    if ((inputWord.length() >= minLength) && (inputWord != "") && (ignoreWords.contains(inputWord) == false)) {
      0
    } else {
      1
    }
  }

}

