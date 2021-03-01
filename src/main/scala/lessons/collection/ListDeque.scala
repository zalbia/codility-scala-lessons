package lessons.collection

import scala.collection.mutable.ListBuffer

/**
 * A minimal deque implementation. Allows rebuilding as an all-left, all-right,
 * at the middle, or at some pivot.
 *
 * Queue code copied from {@link scala.collection.immutable.Queue}
 */
final case class ListDeque[+A] private (
  private val left: List[A],
  private val right: List[A]
) extends Iterable[A] {
  private lazy val all: List[A] = left ++ right.reverse

  def iterator: Iterator[A] = all.iterator

  override def isEmpty: Boolean = left.isEmpty && right.isEmpty

  /** Add element to the left end of the deque */
  def pushLeft[B >: A](elem: B): ListDeque[B] = copy(elem :: left, right)

  /** Add element to the right end of the deque */
  def pushRight[B >: A](elem: B): ListDeque[B] = copy(left, elem :: right)

  /** Remove element from the left end of the deque */
  def popLeft: (Option[A], ListDeque[A]) = left match {
    case Nil if right.nonEmpty =>
      val rev = right.reverse
      (rev.headOption, copy(Nil, rev.tail.reverse))
    case a :: as               => (Some(a), copy(as, right))
    case Nil                   => (None, this)
  }

  /** Remove element from the right end of the deque */
  def popRight: (Option[A], ListDeque[A]) = right match {
    case Nil if left.nonEmpty =>
      val rev = left.reverse
      (rev.headOption, copy(rev.tail.reverse, Nil))
    case a :: as              => (Some(a), copy(left, as))
    case Nil                  => (None, this)
  }

  /** Peek element at the left end of the deque */
  def peekLeft: Option[A] = left.headOption.orElse(right.lastOption)

  /** Peek element at the right end of the deque */
  def peekRight: Option[A] = right.headOption.orElse(left.lastOption)

  /** Rebuild as an all-left deque */
  def allLeft: ListDeque[A] =
    new ListDeque[A](left = all, right = Nil)

  /** Rebuild as an all-right deque */
  def allRight: ListDeque[A] =
    new ListDeque[A](left = Nil, right = all.reverse)

  /** Rebuild as a balanced deque with a middle pivot */
  def balanced: ListDeque[A] = balance(size / 2)

  /** Balances a deque around a pivot */
  def balance(pivot: Int): ListDeque[A] = ListDeque(this, pivot)
}

object ListDeque {

  /** Builds up the `ListDeque`, balancing elements between left and right */
  def apply[A](as: Iterable[A], pivot: Int = 0): ListDeque[A] = {
    val left  = ListBuffer[A]()
    val right = ListBuffer[A]()
    var i     = 0
    for (a <- as) {
      if (i < pivot) left.append(a) else right.prepend(a)
      i += 1
    }
    new ListDeque[A](left.toList, right.toList)
  }

  /** Constructs an empty ListDeque */
  def empty[A]: ListDeque[A] = new ListDeque[A](Nil, Nil)
}
