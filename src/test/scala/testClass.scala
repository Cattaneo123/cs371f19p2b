import org.scalatest.FunSuite
import scala.collection.immutable.Queue
import scala.collection.immutable.ListMap
import scala.collection.immutable.Map
import Main._

import org.scalatest.FunSuite

class TestClassAll extends FunSuite {

  def testIgnoreWords(description: String, wordSize: Int, word: String, ignoreList: Array[String], outcome: Int) {
    test(description) {
      val window: inputHandler = new inputHandler(10, wordSize, ignoreList, 1)
      assert(window.checkAcceptableWord(word, wordSize, ignoreList) === outcome)

    }

  }

  def testGroupUp(description: String, input: Queue[String], outcome: ListMap[String, Int]) {
    test(description) {
      val window: inputHandler = new inputHandler(10, 1, Array.empty[String], 5)
      assert(window.groupUp(input) === outcome)
    }

  }

  def testGroupUpMapSize(description: String, input: Queue[String], outcome: Int) {
    test(description) {
      val window: inputHandler = new inputHandler(10, 1, Array.empty[String], 5)
      assert(window.groupUp(input).size === outcome)
    }

  }

  def testWordAdding(description: String) {
    test(description) {
      val window: inputHandler = new inputHandler(10, 2, Array.empty[String], 3)
      val it = Iterator("aaa", "bbb", "ccc", "ddd")
      assert(window.checkStuff(it).length === 5)
    }

  }

  val testEmptArray: Array[String] = Array.empty[String]
  val testIgnoreArray = Array("no", "more", "words")
  val testQueue = Queue("this", "is", "a", "test", "a")
  testIgnoreWords("Testing if short words are ignored", 6, "five", testEmptArray, 1)
  testIgnoreWords("Testing if blacklisted words are ignored", 1, "more", testIgnoreArray, 1)
  testIgnoreWords("Testing if long words are not ignored", 4, "five", testEmptArray, 0)
  testIgnoreWords("Testing if non-blacklisted words are ignored", 1, "aaaaaaa", testIgnoreArray, 0)
  testGroupUp("Testing if the queue is being mapped correctly", testQueue, ListMap("is" -> 1, "this" -> 1, "test" -> 1, "a" -> 2))
  //testCheckArgs("Tests if the args are being input correctly", 10, 2, 10, testIgnoreArray)
  testGroupUpMapSize("Tests that the maps are the right size", testQueue, 4)
  testWordAdding("Tests whether the words are being added")

}
