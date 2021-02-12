package lessons._04.counting_elements._2

// N and M are integers within the range [1..100,000];
// M = a.size
// each element of array A is an integer within the range [1..N + 1]

object Solution {
  // mostly procedural solution
  def solution(n: Int, a: Array[Int]): Array[Int] = { // 100% | O(n + m)
    val counters = Array.ofDim[Int](n)
    var maxCounter = 0
    var offset = 0
    for (elem <- a) {
      if (elem <= n) {
        val counterIdx = elem - 1
        if (counters(counterIdx) < offset)
          counters(counterIdx) = offset + 1
        else
          counters(counterIdx) += 1

        if (counters(counterIdx) > maxCounter)
          maxCounter = counters(counterIdx)
      } else if (elem == n + 1)
        offset = maxCounter
    }
    counters.map(math.max(offset, _))
  }
}

object OopAlternative {
  // "OOP" solution
  def solution(n: Int, a: Array[Int]): Array[Int] = {
    val maxCounters = MaxCounters.create(n)
    a.foreach(elem =>
      if (elem <= n) maxCounters.maxCount(elem)
      else maxCounters.max()
    )
    maxCounters.finalCounter()
  }

  // could declare inside solution, just wouldn't read as well
  private[_2] class MaxCounters private(
    private val counters: Array[Int],
    private var maxCounter: Int = 0,
    private var offset: Int = 0
  ) {
    def maxCount(elem: Int): Unit = {
      if (count(elem) < offset)
        setCount(elem)(offset + 1)
      else
        setCount(elem)(count(elem) + 1)

      if (count(elem) > maxCounter)
        maxCounter = count(elem)
    }

    private def count(elem: Int): Int =
      counters(elem - 1)
    private def setCount(elem: Int)(value: Int): Unit =
      counters(elem - 1) = value

    def max(): Unit = offset = maxCounter

    def finalCounter(): Array[Int] = counters.map(math.max(offset, _))
  }

  private[_2] object MaxCounters {
    def create(n: Int) = new MaxCounters(Array.ofDim(n))
  }
}