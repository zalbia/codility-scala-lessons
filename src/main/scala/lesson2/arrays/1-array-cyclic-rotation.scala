package lesson2.arrays

import scala.annotation.tailrec

object Solution {
  // N and K are integers within the range [0..100];
  // each element of array A is an integer within the range [−1,000..1,000].
  @tailrec
  def solution(a: Array[Int], k: Int): Array[Int] = { // 100%
    if (k == 0 || k == a.length) a
    else solution(a.last +: a.dropRight(1), k - 1)
  }
}

import scala.util.Random
import scala.util.{Failure, Success}
import utest._

val random = new Random()
val f = Solution.solution _

TestRunner.run(Tests {
  test { assert(f(Array(3, 8, 9, 7, 6), 3) sameElements Array(9, 7, 6, 3, 8)) }
  test { assert(f(Array(1, 2, 3, 4), 4) sameElements Array(1, 2, 3, 4)) }
  test { assert(f(Array(1, 2, 3, 4), 3) sameElements Array(2, 3, 4, 1)) }

}).leaves.map(_.value).foreach {
  case Failure(exception) => println(exception)
  case Success(value) => println(value)
}
