object Solution {
  final case class Gap(biggest: Int, current: Int)

  def solution(n: Int): Int = {
    n.toBinaryString.foldLeft(Gap(0, 0)) { (gap, n) =>
      n match {
        case '0' =>
          val current = gap.current + 1
          gap.copy(if (current > gap.biggest) current else gap.biggest, current)
        case '1' => gap.copy(current = 0)
        case _ => ??? // can't happen. could be type-safe with an ADT
      }
    }.biggest
  }
}

import Solution.solution

solution(0)     // 0              1
solution(1)     // 1              0
solution(2)     // 10             1
solution(9)     // 1001           2
solution(6)     // 1010           1
solution(37)    // 100101         2
solution(273)   // 100010001      3
solution(4375)  // 1000100010111  3
solution(4096)  // 1000000000000  12
