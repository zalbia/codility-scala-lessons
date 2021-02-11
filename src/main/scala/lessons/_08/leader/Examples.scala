package lessons._08.leader

object Examples {
  type Elem = Int
  type Count = Int

  // slow because Map
  def slowLeader(a: Array[Int]): Int = { // T: O(n * eC) | S: O(n)
    if (a.isEmpty) -1
    else {
      a.foldLeft(Map.empty[Elem, Count]) { (counts, elem) =>
        counts + (elem -> counts.get(elem).map(_ + 1).getOrElse(1))
      }.maxBy(_._2) match {
        case (_, count) if count <= a.length / 2 => -1
        case (elem, _) => elem
      }
    }
  }

  // code practically speaks for itself
  def fastLeader(a: Array[Int]): Int = // T: O(n log n) | S: O(n)
    if (a.isEmpty) -1
    else {
      val candidate = a.sorted.apply(a.length / 2)
      if (a.count(_ == candidate) > a.length / 2) candidate else -1
    }

  // a leader should outnumber the occurrences of every other element
  // stack can contain the candidate at the end if there is any
  def goldenLeaderStack(a: Array[Int]): Int = { // T: O(n) | S: O(n)
    val stack = a.foldLeft(List.empty[Int]) { // initial value doesn't matter
      case (Nil, elem) => elem :: Nil // push
      case (stack@top :: _, elem) if top == elem => elem :: stack // peek & push
      case (_ :: pop, _) => pop
    }
    stack.headOption.filter(c => a.count(_ == c) > a.length / 2).getOrElse(-1)
  }

  // a leader should outnumber the occurrences of every other element
  // you're left with a size and the mode
  // reduce the stack to just a size and the top element
  def goldenLeader(a: Array[Int]): Int = { // T: O(n) | S: O(1)
    val (size, mode) = a.foldLeft((0, Int.MinValue)) { // initial value doesn't matter
      case ((0, _), elem) => (1, elem)
      case ((size, value), elem) if value == elem => (size + 1, value)
      case ((size, value), _) => (size - 1, value)
    }
    val candidate = if (size > 0) Some(mode) else None // stack headOption
    candidate.filter(c => a.count(_ == c) > a.length / 2).getOrElse(-1)
  }
}

import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()

  import Examples._

  val solutions = Array(slowLeader _, fastLeader _, goldenLeader _, goldenLeaderStack _)

  val tests = Tests {
    test("example") { check(Array(6, 8, 4, 6, 8, 6, 6), 6) }
    test { check(Array(), -1) }
    test { check(Array(1), 1) }
    test { check(Array(0, 0), 0) }
    test { check(Array(0, 1), -1) }
    test { check(Array(1, 0), -1) }
    test { check(Array(0, 1, 0), 0) }
    test { check(Array(0, 1, 1), 1) }
    test { check(Array(1, 0, 1), 1) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    solutions.foreach { f =>
      val result = f(a)
      assert(result == expected)
    }
  }
}

