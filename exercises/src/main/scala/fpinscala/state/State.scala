package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = State.Rand[A]

  val int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt: Rand[Int] = {
    int.map(i => if (i < 0) -(i + 1) else i)
  }

  def double: Rand[Double] = nonNegativeInt.map(i => i.toDouble/Int.MaxValue)

  def intDouble: Rand[(Int,Double)] = int.map2(double)((_,_))

  def doubleInt = double.map2(int)((_,_))

//  def double3(rng: RNG): ((Double,Double,Double), RNG) = RNG.map2(both(double, double), double){
//    case ((d1,d2), d3) => (d1,d2,d3)
//  }(rng)
//
//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)
//
//    def map2fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//      flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
//
//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
//    val (a, r1) = ra(rng)
//    val (b, r2) = rb(r1)
//    (f(a,b), r2)
//  }
//
//  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_,_))
//
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft((rng: RNG) => (List.empty[A], rng))(map2(_,_)(_:+_))
//
//
//  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
//    val (a,r) = f(rng)
//    g(a)(r)
//  }
//
//  def nonNegativeLessThan(n: Int): Rand[Int] = {
//
//    flatMap(nonNegativeInt)(i => if(i + (n-1) - (i%n) >= 0) unit(i%n) else nonNegativeLessThan(n) )
//  }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      a <- this
      b <- sb
    } yield f(a, b)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldLeft(State((s: S) => (List.empty[A], s)))(_.map2(_)(_:+_))
  }

  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
