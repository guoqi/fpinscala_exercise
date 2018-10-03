package check
import random._
import scala.language.implicitConversions

import scala.collection.immutable.Stream.Empty


case class Gen[+A](sample: State[RNG, A]) {

    def map[B](f: A => B): Gen[B] = Gen(sample.map(a => f(a)))

    // Exercise 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

    def **[B](b: Gen[B]): Gen[(A, B)] = Gen(sample ** b.sample)

    // Exercise 8.10
    def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
    // Exercise 8.11
    def apply(n: Int): Gen[A] = forSize(n)

    def map[B](f: A => B): SGen[B] = SGen(n => this(n).map(f))

    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => this(n).flatMap(f(_)(n)))
}

trait Result {
    def isFalsified: Boolean
}

case object Passed extends Result {
    override def isFalsified: Boolean = false
}

case class Falsified(failure: String, successes: Int) extends Result {
    override def isFalsified: Boolean = true
}

// property associated with generator and rule
case class Prop(run: (Int, Int, RNG) => Result) {
    val DEFAULT_CASE_NUM = 100

    // Exercise 8.9
    def && (p: Prop): Prop = Prop((max, n, rng) => run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case e => e
    })

    def || (p: Prop): Prop = Prop((max, n, rng) => run(max, n, rng) match {
        case Falsified(_, _) => p.run(max, n, rng)
        case r => r
    })

    def check(): Unit = run(DEFAULT_CASE_NUM, 5, SimpleRNG(100)) match {
        case Passed => println("%d cases passed ok".format(DEFAULT_CASE_NUM))
        case Falsified(failure, successes) => println("failed after %d passed\nfailure case: %s".format(successes, failure))
    }
}

// Exercise 8.3
case class FakeProp(check: () => Boolean) {
    def && (p: FakeProp): FakeProp = FakeProp(() => this.check() && p.check())
}

object Gen {

    // Exercise 8.5
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ >= 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

    def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] = g.flatMap(f)

    // Exercise 8.12
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

    // Exercise 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.intRange(start, stopExclusive)))

    // Exercise 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

    // Exercise 8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(RNG.double)).flatMap(r => {
        val total = g1._2 + g2._2
        if (r >= g1._2 / total) g1._1 else g2._1
    })

}

object Prop {
    /**
    def forAll[A](g: Gen[A])(f: A => Boolean): Prop = {
        def runOnce(total: Int, n: Int, rng: RNG): Result = {
            if (n == total) if (f(g.sample(rng)._1)) Passed else Falsified(rng.toString, n - 1)
            else {
                val r = g.sample(rng)
                if (! f(r._1)) Falsified(rng.toString, n)
                else runOnce(total, n + 1, r._2)
            }
        }
        Prop((n, rng) => runOnce(n, 1, rng))
    }
    */
    // important helper function, especially in **State actions**
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case None => Empty
        case Some(x) => x._1 #:: unfold(x._2)(f)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = unfold(rng)(rng => Some(g.sample(rng)))

    def forAll[A](ag: Gen[A])(f: A => Boolean): Prop = Prop(
        (_, n, rng) =>
            randomStream(ag)(rng).zip(Stream.from(0)).take(n).map({
                case (a, i) => try {
                    if (f(a)) Passed else Falsified(a.toString, i)
                } catch { case e: Exception => Falsified(a.toString + e.toString, i) }
            }).find(_.isFalsified).getOrElse(Passed)
    )

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop(
        (max, n, rng) => {
            val casesPerSize = (n + max - 1) / max
            val props: Stream[Prop] = Stream.from(0).take(n min max + 1).map(i => forAll(g(i))(f))
            val prop: Prop = props.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))).toList.reduce(_ && _)
            prop.run(max, n, rng)
        }
    )
}
