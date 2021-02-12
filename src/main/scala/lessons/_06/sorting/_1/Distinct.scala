package lessons._06.sorting._1

// N is an integer within the range [0..100,000];
// each element of array A is an integer within the range [−1,000,000..1,000,000].
object Solution {
  // you _could_ map, but you'd end up having to allocate a 2-billion-element array. fuck that.
  def solution(a: Array[Int]): Int = // 100% | time: O(n log n) | space: O(n)
    if (a.length <= 1) a.length
    else 1 + a.sorted.iterator.sliding(2).count(l => l(0) != l(1))
}
