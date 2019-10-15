import scala.collection.immutable.Queue
import scala.collection.immutable.Map
import scala.collection.immutable.ListMap
import sun.misc.Signal

object Main {
  val SIZE_OF_DISPLAYED_MAP: Int = 10
  val SIZE_OF_WORDS: Int = 6
  val SIZE_OF_WINDOW: Int = 1000
  val BAD_WORDS: Array[String] = Array.empty[String]

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
    window.checkStuff(words)

  }

}

class inputHandler(queueSize: Int, wordSize: Int, wordCheck: Array[String], mapSize: Int) {
  //These are the scanLeft operations we need to use
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

  def checkStuff(input: Iterator[String]): Int = {
    val runningTotal = input.scanLeft(Queue.empty[String])(queueAdding)
    val rntSize = runningTotal.size - 1
    runningTotal.foreach { groupUp }
    rntSize
  }
  def groupUp(input: Queue[String]): ListMap[String, Int] = {
    val freq = input.groupBy(identity).mapValues(_.size)
    val sortedFreq = ListMap(freq.toSeq.sortWith(_._2 < _._2): _*)
    val sizeFreq = sortedFreq.drop(sortedFreq.size - mapSize)
    println("Words in ascending order of appearance")
    sizeFreq.foreach(println)
    sizeFreq
  }

  def checkAcceptableWord(inputWord: String, minLength: Int, ignoreWords: Array[String]): Int = {
    if ((inputWord.length() >= minLength) && (inputWord != "") && (ignoreWords.contains(inputWord) == false)) {

      0
    } else {
      1
    }
  }

}

