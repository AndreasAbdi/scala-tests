package four

import scala.collection.mutable.ListBuffer

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE, B] = this match {
    case Left(a) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] = (this, b) match {
    case (Left(a), _) => Left(a)
    case (_, Left(b)) => Left(b)
    case (Right(a), Right(b))=> Right(f(a,b))
  }

  //or with syntax sugar
  def map2New[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] = {
    for {
      aa <- this
      bb <- b
    } yield (f(aa,bb))
  }

  def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] = {
    es.foldRight[Either[E,List[A]]](Right(Nil))((a,b) => a.map2(b)(_::_))
  }

  def traverse[E, A, B](es: List[Either[E,A]])(f: A => Either[E,B]): Either[E, List[B]] = {
    es.foldRight[Either[E,List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_::_))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Main {



  def main(args: Array[String]): Unit = {
    println("either tests")
    var evals = new ListBuffer[Any]()

    val listevals = evals.toList
    println("object instantiated")
    evals.foreach(println)
  }
}
