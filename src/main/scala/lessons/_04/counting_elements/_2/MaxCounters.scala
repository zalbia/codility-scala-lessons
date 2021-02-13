package lessons._04.counting_elements._2

// N and M are integers within the range [1..100,000];
// M = a.size
// each element of array A is an integer within the range [1..N + 1]

object Solution {
  // mostly procedural solution
  def solution(n: Int, a: Array[Int]): Array[Int] = { // 100% | O(n + m)
    val counters   = Array.ofDim[Int](n)
    var maxCounter = 0
    var offset     = 0
    for (elem <- a)
      if (elem <= n) {
        val elemIdx = elem - 1
        if (counters(elemIdx) < offset) //
          counters(elemIdx) = offset + 1
        else
          counters(elemIdx) += 1

        if (counters(elemIdx) > maxCounter)
          maxCounter = counters(elemIdx)
      } else if (elem == n + 1)
        offset = maxCounter
    counters.map(math.max(offset, _))
  }
}

object OopAlternative {
  // virtually same perf as procedural because it _is_ procedural
  def solution(n: Int, a: Array[Int]): Array[Int] = {
    val maxCounters = MaxCounters(n)
    a.foreach(elem => // foreach is just an impure fold
      if (elem <= n) maxCounters.count(elem)
      else maxCounters.max()
    )
    maxCounters.finalCounter()
  }

  // could declare inside solution method, just wouldn't read as well
  private[_2] final class MaxCounters private (
    private val counters: Array[Int],
    private var maxCounter: Int = 0,
    private var totalOffset: Int = 0
  ) {
    def count(elem: Int): Unit = { // 2 statements
      if (currentCount(elem) < totalOffset)
        setCount(elem)(totalOffset + 1)
      else
        setCount(elem)(currentCount(elem) + 1)
      updateMaxCounter(elem)
    }

    private def updateMaxCounter(elem: Int): Unit     =
      maxCounter = math.max(currentCount(elem), maxCounter)
    private def currentCount(elem: Int): Int          =
      counters(elem - 1)
    private def setCount(elem: Int)(value: Int): Unit =
      counters(elem - 1) = value

    def max(): Unit                = totalOffset = maxCounter
    def finalCounter(): Array[Int] = counters.map(math.max(totalOffset, _))
  }

  private[_2] object MaxCounters {
    def apply(n: Int) = new MaxCounters(Array.ofDim(n))
  }
}

object FpAlternative {
  // allocation city, but this isn't zero-cost-abstractions-ville
  // vector as a replacement for arrays
  def solution(n: Int, a: Array[Int]): Array[Int] = // 100% | O(n + m) | O(n)
    a.foldLeft(MaxCounters(n)) { (maxCounters, elem) => // fold is just a pure foreach
      if (elem <= n) maxCounters.count(elem)
      else maxCounters.max
    }.finalCounter
      .toArray

  final case class MaxCounters private (
    counters: Vector[Int],
    currentMax: Int = 0,
    totalOffset: Int = 0
  ) {
    def count(elem: Int): MaxCounters = { // 2 expressions
      val elemCounted =
        if (currentCount(elem) < totalOffset)
          setCount(elem)(totalOffset + 1)
        else
          setCount(elem)(currentCount(elem) + 1)
      elemCounted.updateMaxCounter(elem)
    }

    def updateMaxCounter(elem: Int): MaxCounters     =
      copy(currentMax = math.max(currentCount(elem), currentMax))
    def currentCount(elem: Int): Int                 =
      counters(elem - 1)
    def setCount(elem: Int)(value: Int): MaxCounters =
      copy(counters.updated(elem - 1, value))

    // without lazy, causes stack overflow
    lazy val max          = copy(totalOffset = currentMax)
    // without lazy, finalCounter is eagerly computed for every copy call (!!!)
    lazy val finalCounter = counters.map(math.max(totalOffset, _))
  }

  object MaxCounters {
    def apply(n: Int) = new MaxCounters(Vector.fill(n)(0))
  }
}
