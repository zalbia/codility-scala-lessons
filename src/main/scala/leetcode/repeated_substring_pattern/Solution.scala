package leetcode.repeated_substring_pattern

object Solution {
  def repeatedSubstringPattern(s: String): Boolean =
    (s.length / 2 to 1 by -1)
      .filter(s.length % _ == 0)
      .map(s.grouped(_))
      .exists { group =>
        val list = group.toList
        list.forall(_ == list.head)
      }
}
