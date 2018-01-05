package two

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if ( n <= 0) acc
      else go(n-1, acc*n)
    go(n, 1)
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-25))
    println(formatFactorial(12))
}