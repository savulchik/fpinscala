package fpinscala.state

import scala.annotation.tailrec


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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, next) = rng.nextInt
    val r =
      if (i >= 0) i
      else if (i == Int.MinValue) 0
      else -i
    (r, next)
  }

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt) { nni =>
      if (nni == Int.MaxValue) 0d
      else nni.toDouble / Int.MaxValue
    }(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, next1) = rng.nextInt
    val (d, next2) = double(next1)
    ((i, d), next2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, next1) = double(rng)
    val (d2, next2) = double(next1)
    val (d3, next3) = double(next2)
    ((d1, d2, d3), next3)
  }

  def ints1(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(acc: List[Int], count: Int, state: RNG): (List[Int], RNG) = {
      if (count == 0) (acc, state)
      else {
        val (i, next) = state.nextInt
        loop(i :: acc, count - 1, next)
      }
    }
    loop(List.empty[Int], count, rng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (r, acc) =>
      map2(r, acc)(_ :: _)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap { a => unit(f(a)) }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b =>
        f(a, b)
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](Nil)) { (sa, acc) =>
      sa.map2(acc)(_ :: _)
    }

  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def action(input: Input) = modify[Machine] { m =>
      input match {
        case Coin if m.locked && m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
        case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
        case _ => m
      }
    }
    val zero = get[Machine].map(m => (m.coins, m.candies))
    inputs.foldRight(zero) { (input, acc) =>
      action(input).flatMap(_ => acc)
    }
  }
}
