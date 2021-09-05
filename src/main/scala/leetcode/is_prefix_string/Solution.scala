package leetcode.is_prefix_string

object Solution {
  def isPrefixString(s: String, words: Array[String]): Boolean =
    words.scanLeft(0)(_ + _.length).drop(1).contains(s.length) &&
      words.iterator.flatMap(_.toIterator).take(s.length).sameElements(s.iterator)
}
