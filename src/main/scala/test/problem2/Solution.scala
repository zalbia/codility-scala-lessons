package test.problem2

import scala.collection.mutable

object Solution {
  def solution(s: String): Int = {
    val alphabetLength = 26
    val letterCounts   = Array.ofDim[Int](alphabetLength)
    for (ch <- s)
      letterCounts(ch - 'a') += 1
    val countSet   = mutable.HashSet[Int]()
    var minDeleted = 0
    for (i <- 0 until alphabetLength if letterCounts(i) != 0)
      if (!countSet(letterCounts(i))) {
        countSet += letterCounts(i)
      } else {
        var deletionCount = 0
        while (countSet(letterCounts(i))) {
          letterCounts(i) -= 1
          deletionCount += 1
        }
        if (letterCounts(i) != 0)
          countSet += letterCounts(i)
        minDeleted += deletionCount
      }
    minDeleted
  }
}

// Test cases
//Solution.solution("aaaabbbb")
//Solution.solution("ccaaffddecee")
