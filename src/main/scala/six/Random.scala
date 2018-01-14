package six
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val targetInt = if(nextInt < 0) -nextInt else nextInt
    (targetInt, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt

    val intermediateDouble = math.abs(nextInt) / Int.MaxValue.toDouble
    val targetDouble = if(intermediateDouble == 1.0) 0 else intermediateDouble
    (targetDouble, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (targetInt, rng2) = rng.nextInt
    val (targetDouble, rng3) = double(rng2)
    ((targetInt, targetDouble), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (targetDouble, rng2) = double(rng)
    val (targetInt, rng3) = rng2.nextInt
    ((targetDouble, targetInt), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (targetDouble, rng2) = double(rng)
    val (targetDouble2, rng3) = double(rng2)
    val (targetDouble3, rng4) = double(rng3)
    ((targetDouble, targetDouble2, targetDouble3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (targetInt, rng2) = rng.nextInt
      val (targetList, rng3) = ints(count-1)(rng2)
      (targetList ++ List(targetInt) , rng3)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))
  //def double(rng: RNG): (Double, RNG) = //

  def doubleViaMap: Rand[Double] = {
    map(int)(i => math.abs(i) / Int.MaxValue.toDouble)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs match {
      case head :: tail => {
        val (state: List[A], rng2) = head(rng)
        val (next_state: List[A], rng3) = sequence(tail)(rng2)
        val t: List[A] = state ++ next_state
        (t, rng3)
      }
      case Nil => (Nil, rng)
    }
  }

  //this is nice.
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((rand, target) => map2(rand, target)(_ :: _))
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    val elems = List.fill(count)(1).map(_ => (rand:RNG) => rand.nextInt)
    sequence2(elems)
  }

  def intsCleaner(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f.apply(rng)
      val randB = g(a)
      randB.apply(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)((i:Int) => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)((a: A) => {
      unit(f(a))
    })
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    flatMap(ra)(a => {
      map(rb)(b => {
        f(a,b)
      })
    })
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1) //[0,5] -> [1,6]

}

case class State[S, +A](run: S => (A,S)) {

  def map[B](f: A => B): State[S,B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B, C](sb: State[S, B])(f: (A,B) => C): State[S, C] = {
    flatMap(a => {
      sb.map(b => {
        f(a,b)
      })
    })
  }

  def flatMap[B](g: A => State[S, B]): State[S,B] = {
    State(seed => {
      val (a, seed2) = this.run(seed)
      g(a).run(seed2)
    })
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(state => (a, state))

  def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](List[A]()))((state, target) => state.map2(target)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()

  def get[S]: State[S, S] = State(s => (s,s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    val result_machine = inputs.foldRight(this)((input, machine) => {machine.update(input)})
    State.unit((result_machine.coins, result_machine.candies))
  }

  def update(i: Input): Machine = {
    (i, this) match {
      case (_, Machine(_, 0, _ )) => this //machine that's out of candies ignores all inputs
      case (Coin, Machine(false, _, _)) => this //adding a coin on a unlocked machine does nothing
      case (Turn, Machine(true, _, _)) => this //adding a turn on a locked machine does nothing
      //insert coin to locked machine will unlock it
      case (Coin, Machine(true, state_candies, state_coins)) => Machine(false, state_candies, state_coins+1)
      //turning knob on unlocked machine will lock it and release a candy
      case (Turn, Machine(false, state_candies, state_coins)) => Machine(true, state_candies-1, state_coins)
    }
  }

}

// solution from book. How does the machine get submitted to the simulate machine function?
// oooh, so you pass the initial state of your object for the run and then it executes.
// that's pretty clever. 
object CandyBox {

  def update = (i: Input) => (m: Machine) => {
    (i, m) match {
      case (_, Machine(_, 0, _ )) => m //machine that's out of candies ignores all inputs
      case (Coin, Machine(false, _, _)) => m //adding a coin on a unlocked machine does nothing
      case (Turn, Machine(true, _, _)) => m //adding a turn on a locked machine does nothing
      //insert coin to locked machine will unlock it
      case (Coin, Machine(true, state_candies, state_coins)) => Machine(false, state_candies, state_coins+1)
      //turning knob on unlocked machine will lock it and release a candy
      case (Turn, Machine(false, state_candies, state_coins)) => Machine(true, state_candies-1, state_coins)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(input => State.modify[Machine](update(input))))
    s <- State.get
  } yield (s.coins, s.candies)
}

object Main {

  def main(args: Array[String]): Unit = {
    println("Test random states")
    val obj = CandyBox.simulateMachine(List(Coin, Turn, Coin, Turn))
    val result = obj.run(Machine(true,2,3))
    println("hi")
  }
}
