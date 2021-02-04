object Solution {
  def solution(A: Array[Int]): Int = // 100%
    A.sorted.iterator.filter(_ > 0).fold(1)((smallest, a) => {
      if (smallest == a) a + 1 else smallest
    })
}

val soln = Solution.solution _

soln(Array(1, 3, 6, 4, 1, 2))
soln(Array(1, 2, 3))
soln(Array(-1, -3))
soln(Array(-100000,100000))
