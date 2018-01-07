package three

import scala.collection.mutable.ListBuffer
import scala.math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => max(maximum(left), maximum(right))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + max(depth(left), depth(right))
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](tree: Tree[A])(to: A => B)(merge: (B,B) => B): B = tree match {
    case Leaf(a) => to(a)
    case Branch(left, right) => merge(fold(left)(to)(merge), fold(right)(to)(merge))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)((_)=> 1)((a: Int, b: Int) => a + b)
  }

  def maxViaFold[A](tree: Tree[A])(maximum: (A,A) => A): A = {
    fold(tree)((a)=> a)((a, b) => maximum(a,b))
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)((_)=> 0)((a: Int, b: Int) => 1 + max(a,b))
  }

  def mapViaFold[A,B](tree: Tree[A])(op: A => B): Tree[B] = {
    fold(tree)((a)=> Leaf(op(a)): Tree[B])(Branch(_,_))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("tree tests")
    val tree = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(5)))
    var evals = new ListBuffer[Any]()
    evals += Tree.size(tree)
    evals += Tree.maximum(tree)
    evals += Tree.depth(tree)
    val listevals = evals.toList
    println("object instantiated")
    evals.foreach(println)
  }
}

