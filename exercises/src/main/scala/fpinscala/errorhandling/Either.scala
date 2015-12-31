package fpinscala.errorhandling


import fpinscala.errorhandling.Some

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(x: A) => Right(f(x))
   case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(x: A) => f(x)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(x) => Right(x)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(a => b.map(b => f(a,b)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](input: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {

    def appendEithBs(eitherB: Either[E, B], eitherBs: Either[E, List[B]]): Either[E, List[B]] = {
      eitherB.map2(eitherBs)(_ :: _)
    }

    input.foldRight[Either[E, List[B]]](Right(Nil))((a, eitherBs) => appendEithBs(f(a), eitherBs))
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}