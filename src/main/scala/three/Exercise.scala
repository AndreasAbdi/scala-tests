package three;

import scala.collection.mutable.ListBuffer

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, remainder) => remainder
  }

  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case Cons(_, remainder) => {
      if (n > 0) drop(remainder, n-1)
      else list
    }
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, remainder) => {
      if (f(head)) dropWhile(remainder, f)
      else list
    }
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, remainder) => Cons(head, remainder)
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(x, remainder) => Cons(x,init(remainder))
  }

  def foldRight[A, B] (as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a,b) => b+1)
  }

  def foldLeft[A, B] (as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
    }
  }

  //wrap identity function with functions recursively and then eval.
  def foldLeftViaRight[A, B] (as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b:B) => b)((a, g) => b => g(f(b,a)))(z)
  }

  //same thing as above, but the other way around.
  def foldRightViaLeft[A, B] (as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }

  def sum2(ns: List[Int]) = {
    foldLeft(ns, 0)(_+_)
  }

  def product(ns: List[Double]) = {
    foldLeft(ns, 1.0)(_*_)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((b: Int,_) => b + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((a,b) => Cons(b,a))
  }


  def reverse2[A](as: List[A]): List[A] = {
    foldLeftViaRight(as, List[A]())((a,b) => Cons(b,a))
  }

  def append[A](as:List[A], b: A): List[A] = as match {
    case Nil => Cons(b,Nil)
    case Cons(a, Nil) => Cons(a,Cons(b,Nil))
    case Cons(a, remainder) => Cons(a,append(remainder, b))
  }

  def append2[A](as: List[A], end:List[A]): List[A] = {
    foldRightViaLeft(as, end)((a,b) => Cons(a,b))
  }

  def concat[A](lists:List[List[A]]):List[A] = {
    foldRightViaLeft(lists, Nil:List[A])(append2)
  }

  def addOne(ns: List[Int]): List[Int] = ns match {
    case Nil => Nil
    case Cons(a,b) => Cons(a+1, addOne(b))
  }

  def stringify(ns: List[Double]): List[String] = {
    foldLeft(ns, Nil:List[String])((b, a) => Cons(a.toString, b))
  }

  def map[A,B](as: List[A], f: A => B): List[B] = {
    foldLeft(as, Nil:List[B])((b,a) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, Nil:List[A])((b,a) => {
      if(f(a)) Cons(a,b)
      else b
    })
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as, f))
  }

  def FilterViaFlatMap[A,B](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if(f(a)) List(a) else Nil)
  }

  def addLists(as: List[Int], bs: List[Int]): List[Int] = (as,bs) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1,a2),Cons(b1,b2)) => Cons(a1 + b1, addLists(a2,b2))
  }

  def zipWith[A](as: List[A], bs: List[A])( f: (A,A) => A): List[A] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1,a2),Cons(b1,b2)) => Cons(f(a1,b1), zipWith(a2,b2)(f))
  }

  def hasSubsequence[A](sub: List[A], sup:List[A]): Boolean = (sub, sup) match {
    case (Nil, Nil) => true
    case (Nil, _) => true
    case (_, Nil) => false
    case (Cons(a1,a2),Cons(b1,b2)) => {
      if (a1 == b1) hasSubsequence(a2, b2) || hasSubsequence(Cons(a1,a2), Cons(a1,a2))
      else hasSubsequence(Cons(a1,a2),b2)
    }
  }

  def forEach[A](as: List[A], f: (A)=>Unit): Unit = {
    as match {
      case Nil => {}
      case Cons(a,b) => {
        f(a)
        forEach(b,f)
      }
    }
  }
}

object Exercise {

  def main(args: Array[String]): Unit = {
    val a = List(1,2,3)
    var evals = new ListBuffer[Any]()
    evals += List.tail(a)
    evals += List.setHead(a, 4)
    evals += List.drop(a,2)
    evals += List.dropWhile(a,(i:Int) => i < 2)
    evals += List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    evals += List.reverse(a)
    evals += List.reverse2(a)
    evals += List.length(a)
    evals += List.length2(a)
    evals += List.concat(List(List(a), List(4,5,6)))
    evals += List.addOne(a)
    evals += List.stringify(List(1.2,3.4,5.12))
    evals += List.hasSubsequence(List(1,2,3), List(1,2,1,2,3,4,5))
    evals += List.hasSubsequence(List(1,2,3), List(1,2,3,4,5))
    evals += List.hasSubsequence(List(1,2,3), List(2,1,2,3,4,5))
    evals += List.hasSubsequence(List(1,2,3), List(3,4,5))
    val listevals = evals.toList
    println("object instantiated")
    println(a)
    evals.foreach(println)

  }
}
