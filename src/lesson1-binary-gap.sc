object Solution {
  def solution(n: Int): Int = { // 100%
    var biggest = 0
    var current = 0
    n.toBinaryString.foreach { n => {
      n match {
        case '0' => current += 1
        case '1' => current = 0
      }
      if (current > biggest) biggest = current
    }}
    biggest
  }
}

import Solution.solution

solution(0)    // 0              1
solution(1)    // 1              0
solution(2)    // 10             1
solution(9)    // 1001           2
solution(6)    // 1010           1
solution(37)   // 100101         2
solution(273)  // 100010001      3
solution(4375) // 1000100010111  3
solution(4096) // 1000000000000  12
