trait RNG {
    def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

object RNG {
    type Rand[+A] = RNG => (A, RNG)

    def int: Rand[Int] = rng => rng.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
        val (a, r) = s(rng)
        (f(a), r)
    }

    // Exercise 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val a = rng.nextInt
        if (a._1 < 0) {
            (a._1 + Int.MaxValue + 1, a._2)
        } else {
            a
        }
    }

    // Exercise 6.2
    def double(rng: RNG): (Double, RNG) = {
        val a = nonNegativeInt(rng)
        (a._1.toDouble / Int.MaxValue.toDouble, a._2)
    }

    // Exercise 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val a = nonNegativeInt(rng)
        val b = double(a._2)
        ((a._1, b._1), b._2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) =  {
        val a = double(rng)
        val b = nonNegativeInt(a._2)
        ((a._1, b._1), b._2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val a = double(rng)
        val b = double(a._2)
        val c = double(b._2)
        ((a._1, b._1, c._1), c._2)
    }

    // Exercise 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        if (count == 0) (Nil, rng)
        else {
            val a = rng.nextInt
            val r = ints(count - 1)(a._2)
            (a._1 :: r._1, r._2)
        }
    }

    // Exercise 6.5
    def double2(rng: RNG): (Double, RNG) = map[Int, Double](nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)(rng)

    // Exercise 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
    }

    // Exercise 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
        case Nil => (Nil, rng)
        case x :: xs => {
            val (a, rng2) = x(rng)
            val (as, rngn) = sequence(xs)(rng2)
            (a :: as, rngn)
        }
    }

    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = sequence[Int](List.fill(count)(int))(rng)

    // Exercise 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
        val (a, rng2) = f(rng)
        val (b, rng3) = g(a)(rng2)
        (b, rng3)
    }

    def nonNegativeLessThan2(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan2(n)
    })

    // Exercise 6.9
    def o_map[A, B](ra: Rand[A])(f: A => B): Rand[B] = flatMap(ra)(a => unit(f(a)))

    def o_map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}
