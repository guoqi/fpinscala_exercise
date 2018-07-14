import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import RNG._
import State._
import Machine._


@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
    val srng = SimpleRNG(1000)

    def check_double(d: Double): Boolean = d >=0 && d < 1

    test("Exercise 6.1") {
        assert(nonNegativeInt(srng) == nonNegativeInt(SimpleRNG(1000)))
        val (a, r) = nonNegativeInt(srng)
        assert(a >= 0)
        val (a2, r2) = nonNegativeInt(r)
        assert(a2 >= 0)
        val (a3, r3) = nonNegativeInt(r2)
        assert(a3 >= 0)
        println((a, a2, a3))
    }

    test("Exercise 6.2") {
        assert(double(srng) == double(SimpleRNG(1000)))
        val (a, r) = double(srng)
        assert(a >= 0 && a < 1)
        val (a2, r2) = double(r)
        assert(a2 >= 0 && a2 < 1)
        val (a3, r3) = double(r2)
        assert(a3 >= 0 && a3 < 1)
        println((a, a2, a3))
    }

    test("Exercise 6.3.1") {
        assert(intDouble(srng) == intDouble(SimpleRNG(1000)))
        val ((i, d), r) = intDouble(srng)
        assert(i >= 0 && d >= 0 && d < 1)
        val ((i2, d2), r2) = intDouble(r)
        assert(i2 >= 0 && d2 >= 0 && d2 < 1)
        val ((i3, d3), r3) = intDouble(r2)
        assert(i3 >= 0 && d3 >= 0 && d3 < 1)
        println(((i, d), (i2, d2), (i3, d3)))
    }

    test("Exercise 6.3.2") {
        assert(doubleInt(srng) == doubleInt(SimpleRNG(1000)))
        val ((d, i), r) = doubleInt(srng)
        assert(i >= 0 && d >= 0 && d < 1)
        val ((d2, i2), r2) = doubleInt(r)
        assert(i2 >= 0 && d2 >= 0 && d2 < 1)
        val ((d3, i3), r3) = doubleInt(r2)
        assert(i3 >= 0 && d3 >= 0 && d3 < 1)
        println(((i, d), (i2, d2), (i3, d3)))
    }

    test("Exercise 6.3.3") {
        assert(double3(srng) == double3(SimpleRNG(1000)))
        val ((a, b, c), r) = double3(srng)
        assert(check_double(a) && check_double(b) && check_double(c))
        val ((a2, b2, c2), r2) = double3(r)
        assert(check_double(a2) && check_double(b2) && check_double(c2))
        val ((a3, b3, c3), r3) = double3(r2)
        assert(check_double(a3) && check_double(b3) && check_double(c3))
        println(((a, b, c), (a2, b2, c2), (a3, b3, c3)))
    }

    test("Exercise 6.4") {
        assert(ints(5)(srng) == ints(5)(SimpleRNG(1000)))
        println(ints(5)(srng))
    }

    test("Exercise 6.5") {
        assert(double2(srng) == double2(SimpleRNG(1000)))
        val (a, r) = double2(srng)
        assert(a >= 0 && a < 1)
        val (a2, r2) = double2(r)
        assert(a2 >= 0 && a2 < 1)
        val (a3, r3) = double2(r2)
        assert(a3 >= 0 && a3 < 1)
        println((a, a2, a3))
    }

    test("Exercise 6.6") {
        val ra = RNG.unit(20)
        val rb = RNG.unit(1)
        val rc = map2(ra, rb)(_ + _)
        assert(rc(srng)._1 == 21)
        assert(rc(srng)._2 == srng)
    }

    test("Exercise 6.7") {
        val s = RNG.sequence(List.fill(4)(RNG.unit(1)))
        assert(s(srng)._1 == List(1, 1, 1, 1))
        assert(s(srng)._2 == srng)

        assert(ints2(5)(srng) == ints2(5)(SimpleRNG(1000)))
        println(ints2(5)(srng))
    }

    test("Exercise 6.8") {
        assert(nonNegativeLessThan2(10)(srng) == nonNegativeLessThan2(10)(SimpleRNG(1000)))
        val (a, r) = nonNegativeLessThan2(10)(srng)
        assert(a >= 0 && a < 10)
        val (a2, r2) = nonNegativeLessThan2(10)(r)
        assert(a2 >= 0 && a2 < 10)
        val (a3, r3) = nonNegativeLessThan2(10)(r2)
        assert(a3 >= 0 && a3 < 10)
        println(a, a2, a3)
    }

    test("Exercise 6.9") {
        val ra = RNG.unit(10)
        val rb = RNG.unit(4)
        assert(map(ra)(_ * 2)(srng) == o_map(ra)(_ * 2)(srng))
        assert(map2(ra, rb)(_ * _)(srng) == o_map2(ra, rb)(_ * _)(srng))
    }

    test("Exercise 6.10") {
        // TODO
    }

    test("Exercise 6.11") {
        val initial = Machine(locked = true, 5, 10)
        val initial2 = Machine(locked = false, 5, 10)
        val inputs = List(Coin, Turn, Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn, Coin, Turn)
        val inputs2 = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
        val inputs3 = List()
        val inputs4 = List(Turn, Turn)
        val inputs5 = List(Coin, Coin, Turn)

        assert(simulateMachine(inputs)(initial)._1 == (14, 1))
        assert(simulateMachine(inputs2)(initial)._1 == (15, 0))
        assert(simulateMachine(inputs3)(initial)._1 == (10, 5))
        assert(simulateMachine(inputs4)(initial)._1 == (10, 5))
        assert(simulateMachine(inputs5)(initial)._1 == (11, 4))

        assert(simulateMachine(inputs)(initial2)._1 == (13, 1))
        assert(simulateMachine(inputs2)(initial2)._1 == (14, 0))
        assert(simulateMachine(inputs3)(initial2)._1 == (10, 5))
        assert(simulateMachine(inputs4)(initial2)._1 == (10, 4))
        assert(simulateMachine(inputs5)(initial2)._1 == (10, 4))
    }
}
