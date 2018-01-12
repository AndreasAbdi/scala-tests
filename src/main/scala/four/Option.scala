package four

import scala.collection.mutable.ListBuffer

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) Some(a) else None
    case None => None
  }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Main {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(getA), Some(getB)) => Some(f(getA, getB))
  }

  def map2ForComp[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.isEmpty) None //is this redundant? probably
    else a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a map f) //huh, you can ignore ( and .
  }

  //so same thing as sequence w/ an operation on the elements before the join
  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((a, b) => map2(f(a), b)(_ :: _))
  }

  def main(args: Array[String]): Unit = {
    println("option tests")
    val options = Some(12)
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    var evals = new ListBuffer[Any]()
    evals += options.filter((a: Int) => a > 5)
    evals += options.map((a: Int) => a + 4)
    evals += options.flatMap((a: Int) => Some(a * 2))
    evals += options.getOrElse(4)
    evals += options.orElse(Some(5))
    evals += mean(seq)
    evals += variance(seq)
    evals += sequence(List(Some(1), Some(2), Some(3)))
    val listevals = evals.toList
    println("object instantiated")
    evals.foreach(println)
  }
}
