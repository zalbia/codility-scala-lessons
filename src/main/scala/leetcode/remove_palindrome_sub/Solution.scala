package leetcode.remove_palindrome_sub

import scala.annotation.tailrec

object Solution {
  def removePalindromeSub(s: String): Int = {
    @tailrec
    def findLeftover(s: String, start: Int, end: Int, leftover: List[Char]): List[Char] =
      if (s.length == 1) leftover
      else {
        val nextBegin     = start + 1
        val lastPairIndex = s.lastIndexOf(s(start), end)
        val nextEnd       = if (lastPairIndex == end) end - 1 else lastPairIndex
        if (start == end)
          leftover
        else {
          findLeftover(s, nextBegin, nextEnd, s.slice(nextEnd + 1, end).toList ::: leftover)
        }
      }

    @tailrec
    def loop(leftover: List[Char], times: Int): Int = {
      println(leftover)
      println(times)
      val length       = leftover.length
      println(length)
      val nextLeftover = findLeftover(leftover.mkString, 0, length, Nil)
      if (nextLeftover.isEmpty)
        times + 1
      else loop(nextLeftover, times + 1)
    }
    loop(s.toList, 0)
  }
}
