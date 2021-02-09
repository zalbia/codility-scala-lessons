object Solution {

  def solution(s: String): Int = // 100% O(n)
    if (balanced(s)) 1
    else 0

  val brackets = Map('{' -> '}', '[' -> ']', '(' -> ')')

  private def balanced(s: String): Boolean = {
    var stack = List.empty[Char]
    for (bracket <- s) {
      bracket match {
        case opening if brackets.contains(opening) =>
          stack = opening :: stack // push
        case closing =>
          if (stack.isEmpty) return false
          val top = stack.head // peek
          if (closing != brackets(top)) return false
          stack = stack.tail // pop
      }
    }
    stack.isEmpty
  }
}

val f = Solution.solution _

f("") // 1
f("{") // 0
f("{[()()]}") // 1
f("([)()]") // 0
f("}") // 0
f("}}}}}") // 0
f("}" * 200000) // 0
f("(" * 200000) // 0
f("(" * 100000 ++ ")" * 100000) // 1
