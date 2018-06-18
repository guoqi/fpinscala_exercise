package Test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import main.App._

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
    test("hhhh") {
        assert(1 == 1)
    }

    test("Exercise 2.1") {
        assert(fib(0) == 0)
        assert(fib(1) == 1)
        assert(fib(2) == 1)
        assert(fib(3) == 2)
        assert(fib(4) == 3)
        assert(fib(5) == 5)
        assert(fib(6) == 8)
        assert(fib(7) == 13)
        assert(fib(8) == 21)
        assert(fib(9) == 34)
        assert(fib(10) == 55)
        assert(fib(11) == 89)
    }

    test("Exercise 2.2") {
        assert(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y))
        assert(! isSorted(Array(1, 3, 2), (x: Int, y: Int) => x < y))
        assert(isSorted(Array(3, 2, 1), (x: Int, y: Int) => x > y))
        assert(isSorted(Array("abc", "acd", "efh", "xzy"), (x: String, y: String) => x < y))
    }

    test("Exercise 2.3") {
        def fn(x: Int, y: Int): Int = x + y

        val cf = curry(fn)
        assert(cf(1)(2) == fn(1, 2))
        assert(cf(3)(10) == fn(3, 10))
    }

    test("Exercise 2.4") {
        def fn(x: Int)(y: Int): Int = x * y

        val uncf = uncurry(fn)
        assert(uncf(1, 2) == fn(1)(2))
        assert(uncf(4, 8) == fn(4)(8))
    }

    test("Exercise 2.5") {
        def f(x: Int): String = x.toString
        def g(x: String): Double = x.toDouble

        def cf = compose(g, f)
        assert(cf(1) == g(f(1)))
        assert(cf(2) == g(f(2)))
    }
}
