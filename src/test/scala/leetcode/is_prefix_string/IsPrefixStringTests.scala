package leetcode.is_prefix_string

import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f      = Solution.isPrefixString _

  val tests = Tests {
    test(check("iloveleetcode", Array("i", "love", "leetcode", "apples"), true))
    test(check("iloveleetcode", Array("apples", "i", "love", "leetcode"), false))
    test(check("a", Array("aa", "aaaa", "bananas"), false))
  }

  def check(s: String, words: Array[String], expected: Boolean): Unit = {
    val result = f(s, words)
    assert(result == expected)
  }
}
