package leetcode.repeated_substring_pattern

import utest._

import scala.util.Random

object RepeatedSubstringPatternTests extends TestSuite {
  val random = new Random()
  val f      = Solution.repeatedSubstringPattern _

  val tests = Tests {
    test(check("a", false))
    test(check("abab", true))
    test(check("aba", false))
    test(check("abcabcabcabc", true))
    test(check("aaaa", true))
//    val min  = Int.MinValue
//    val max  = Int.MaxValue
//    val maxN = 100000
  }

  def check(a: String, expected: Boolean): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
