package leetcode.remove_palindrome_sub

import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f      = Solution.removePalindromeSub _

  val tests = Tests {
    test(check("a", 1))
    test(check("ababa", 1))
    test(check("abb", 2))
    test(check("baabb", 2))
    test(check("babbaabbbaaabbb", 3))
  }

  def check(s: String, expected: Int): Unit = {
    val result = f(s)
    assert(result == expected)
  }
}
