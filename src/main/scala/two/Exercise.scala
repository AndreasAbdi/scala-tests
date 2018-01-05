package two

object Exercise {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, first: Int, second: Int): Int = {
      if(n <= 1) first
      else go(n -1, second, first+second)
    }
    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(index: Int): Boolean = {
      if (index == as.length - 1) true
      else if (!ordered(as(index), as(index+1))) false
      else go(index + 1)
    }

    go(0)
  }


  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a,b))
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println("the fib num of %d is %d".format(6, fib(6)))
    println("the array is sorted: %b".format(isSorted(Array(3,1,2), (a: Int, b: Int) => a < b)))
    println("the array is sorted: %b".format(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b)))

  }
}
