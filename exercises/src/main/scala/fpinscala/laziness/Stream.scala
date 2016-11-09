package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    def rec(as: Stream[A], counter: Int = 0): Stream[A] = as match {
      case Cons(h, t) if counter < n => cons(h(), rec(t(), counter + 1))
      case _ => Empty
    }

    rec(this)
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    def rec(as: Stream[A]): Stream[A] = as match {
      case Cons(h, t) if p(h()) => cons(h(), rec(t()))
      case _ => Empty
    }

    rec(this)
  }

  //5.5
  def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(Option.empty[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: => (A => B)): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: (A => Boolean)): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](bs: Stream[B]): Stream[B] = foldRight(bs)((a, b) => cons(a, b))

  def flatMap[B](f: => (A => Stream[B])): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  //5.13
  def mapUnfold[B](f: => (A => B)): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def takeUnfold(n: Int): Stream[A] = unfold((0, this)) {
    case (c, _) if c == n => None
    case (_, Empty) => None
    case (c, Cons(h, t)) => Some(h(), (c + 1, t()))
  }

  def zipWithUnfold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
    case _ => None
  }

  def zipAllUnfold[B](b: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, b)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (Cons(ha, ta), Empty) => Some((Some(ha()), Option.empty), (ta(), Stream.empty))
    case (Empty, Cons(hb, tb)) => Some((Option.empty, Some(hb())), (Stream.empty, tb()))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean = zipAllUnfold(s).forAll {
    case (Some(a), Some(b)) => a == b
    case (_, Some(_)) => false
    case _ => true
  }

  //5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case a@Cons(h, t) => Some((a, t()))
    case Empty => None
  }

 // 5.16
    def scanRight[B](z: B)(f: => (A, B) => B): Stream[B] = foldRight(Stream(z)){
      case (a, bs @ Cons(b,_)) => cons[B](f(a,b()), bs)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //5.8
  def constant[A](n: A): Stream[A] = Stream.cons(n, constant(n))

  //5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def rec(prev1: Int, prev2: Int): Stream[Int] =  Stream.cons(prev1, rec(prev2, prev1+prev2))

    rec(0,1)
  }

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).fold(Stream.empty[A]){
    case (a, s) => Stream.cons(a, unfold(s)(f))
  }

  //5.12
  val fibsUnfold: Stream[Int] = unfold((0,1)){
    case (a1, a2) => Some((a1, (a2, a1+a2)))
  }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(a => Some((a, a+1)))
  def constantUnfold[A](n: A): Stream[A] = unfold(n)(a => Some(a, a))
  val onesUnfold: Stream[Int] = constantUnfold(1)

}


