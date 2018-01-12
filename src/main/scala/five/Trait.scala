package five


trait Stream[+A] {
  //probably stack traces.
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }


  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {

    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileByFold(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Stream.empty)(
      (a, b) =>
        if (p(a)) Stream.cons(a, b)
        else Stream.empty
    )
  }

  def headOption: Option[A] = {
    foldRight[Option[A]](Option.empty)((a,_) => Option.apply(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight[Stream[B]](Stream.empty[B])((a,b) => Stream.cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Stream.empty[A])((a,b) =>
      if (f(a)) Stream.cons(a, b.filter(f))
      else Stream.empty
    )
  }

  def append[B>:A](dummy: => Stream[B]): Stream[B] = {
    foldRight[Stream[B]](dummy)((a,b) => Stream.cons(a,b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight[Stream[B]](Stream.empty[B])((a,b) => f(a).append(b))
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

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs: Stream[Int] = {
    def go(left: Int, right: Int): Stream[Int] = {
      cons(left+right, go(right, left+right))
    }
    go(0,1)
  }

  //this is confusing.
  def unfold[A, S](base_stream: S)(next_stream: S => Option[(A, S)]): Stream[A] = {
    next_stream(base_stream) match {
      case Some((value, stream)) => cons(value, unfold(stream)(next_stream))
      case None => empty
    }
  }

  def onesFromUnfold: Stream[Int] = {
    unfold(Nil)(_ => Some((1, Nil)))
  }

  def constantFromUnfold[A](a: A): Stream[A] = {
    unfold(a)(b => Some((b, b)))
  }

  def fromFromUnfold(n: Int): Stream[Int] = {
    unfold(n)(b => Some((b, b+1)))
  }

  //wrap left, right in tuple/data object.
  def fibsByUnfold: Stream[Int] = {
    unfold((0,1))(obj => obj match {
      case (a,b) => Some((a, (b, a + b)))
    })
  }

  def mapViaUnfold[A,B](as: Stream[A], f: A => B): Stream[B] = {
    unfold(as)(b => b match {
      case Cons(head, tail) => Some((f(head), tail()))
      case Empty => Option.empty
    })
  }

  def takeViaUnfold[A](as: Stream[A], n: Int): Stream[A] = {
    unfold(as)(stream => stream match {
      case Cons(head, tail) if n > 1 => Some((head(), takeViaUnfold(tail(), n-1)))
      case Cons(head, _) if n == 1 => Some((head(), Stream.empty))
      case _ => Option.empty
    })
  }

  def takeWhileViaUnfold[A](as: Stream[A], p: A => Boolean): Stream[A] = {
    unfold(as)(stream => stream match {
      case Cons(h, t) if p(h()) => Some(h(), t().takeWhile(p))
      case _ => Option.empty
    })
  }

  def zipWith[A](as: Stream[A], bs: Stream[A])( f: (A,A) => A): Stream[A] = {
    unfold((as,bs))(in => in match {
      case (Cons(a1,a2),Cons(b1,b2)) => Some(
        (
          f(a1(),b1()),
          (a2(), b2())
        )
      )
      case _ => Option.empty
    })
  }

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((s1,s2))(in => in match {
      case (Cons(a1,a2), Cons(b1,b2)) => Some(
        (
          (Some(a1()), Some(b1())),
          (a2(), b2())
        )
      )
      case (Stream.empty, Cons(b1, b2)) => Some(
        (
          (Option.empty, Some(b1())),
          (Stream.empty, b2())
        )
      )
      case (Cons(a1,a2), Stream.empty) => Some(
        (
          (Some(a1()), Option.empty),
          (a2(), Stream.empty)
        )
      )
      case (Empty, Empty) => Option.empty
    })
  }

  def startsWith[A](sub: Stream[A], sup: Stream[A]): Boolean = {
    zipAll(sub, sup)
      .takeWhile(!_._2.isEmpty)
      .forAll(in => in match {
        case (a,b) => a == b
      })
  }

  def tails[A](as: Stream[A]): Stream[Stream[A]] = {
    unfold(as)(in => in match {
      case s => Some((s, s.drop(1)))
      case _ => Option.empty
    })
  }

  //?? - solution from book. weird.
  def scanRight[A,B](as: Stream[A], z: B)(f: (A => B) => B): Stream[B] = {
    as.foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1) // eval the actual function operation on the lazy eval'd initial value, and input.
      (b2, cons(b2, p1._2)) // wrap result and tail stream, and return the wrapped object.
    })._2 //return the streams.

  }
}


object TraitMain {
  def main(args: Array[String]): Unit = {
    println("trait tests")
    lazy val ones: Stream[Int] = Stream.cons(1, ones)
    println(List(1,2,3))
    val stream_first: Stream[Int] = Stream.cons(1,Stream.cons(2, Stream.empty))
    println(stream_first.take(5).toList)

  }
}