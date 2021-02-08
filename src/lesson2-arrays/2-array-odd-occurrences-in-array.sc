import scala.collection.mutable

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {

  /**
   * Assumptions:
   *
   * N is an odd integer within the range [1..1,000,000];
   * each element of array A is an integer within the range [1..1,000,000,000];
   * all but one of the values in A occur an even number of times.
   */
  def solution(as: Array[Int]): Int = { // 100%
    val candidates = mutable.Set[Int]()
    as.foreach { a =>
      if (candidates(a)) candidates -= a
      else candidates += a
    }
    candidates.head // head is safe here as there is always 1 answer
  }
}

import Solution._

solution(Array(1))
