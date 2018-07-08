import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Stream._

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {

    val a = Stream(1, 2, 3, 4, 5)
    val e = Stream()

    test("Exercise 5.1") {
        assert(a.toList == List(1, 2, 3, 4, 5))
        assert(e == Empty)
        assert(e.toList == Nil)
    }

    test("Exercise 5.2") {
        assert(a.take(2).toList == List(1, 2))
        assert(a.take(9).toList == List(1, 2, 3, 4, 5))
        assert(e.take(1) == Empty)
    }

    test("Exercise 5.3") {
        assert(a.drop(2).toList == List(3, 4, 5))
        assert(a.drop(10) == Empty)
        assert(e.drop(1) == Empty)
    }

    test("Exercise 5.4") {
        assert(a.forAll(_ > 0))
        assert(! a.forAll(_ % 2 == 0))
        assert(e.forAll(_ => true))
    }

    test("Exercise 5.5") {
        assert(a.takeWhile2(_ < 4).toList == List(1, 2, 3))
        assert(a.takeWhile2(_ < 0).toList == Nil)
        assert(e.takeWhile2(_ => true) == Empty)
    }

    test("Exercise 5.6") {
        assert(a.headOption.contains(1))
        assert(e.headOption.isEmpty)
    }

    test("Exercise 5.7") {
        assert(a.map(_ * 2).toList == List(2, 4, 6, 8, 10))
        assert(e.map(_ => 2) == Empty)

        assert(a.filter(_ % 2 == 0).toList == List(2, 4))
        assert(e.filter(_ => true) == Empty)

        assert(a.append(Stream(1, 2)).toList == List(1, 2, 3, 4, 5, 1, 2))
        assert(a.append(Empty).toList == a.toList)
        assert(e.append(a).toList == a.toList)

        assert(a.flatMap(_ => Stream(8, 9)).toList == List(8, 9, 8, 9, 8, 9, 8, 9, 8, 9))
        assert(e.flatMap(_ => Stream(1, 2)) == Empty)

        assert(a.exists(_ % 2 == 0))
        assert(! e.exists(_ => false))
    }

    test("Exercise 5.8") {
        assert(constant(3).take(4).toList == List(3, 3, 3, 3))
    }

    test("Exercise 5.9") {
        assert(from(2).take(5).toList == List(2, 3, 4, 5, 6))
    }

    test("Exercise 5.10") {
        assert(fibs.take(5).toList == List(1, 1, 2, 3, 5))
    }

    test("Exercise 5.11") {
        assert(unfold(1)(x => if(x > 4) None else Some((x.toString, x + 1))).toList == List("1", "2", "3", "4"))
    }

    test("Exercise 5.12") {
        assert(ones2.take(4).toList == List(1, 1, 1, 1))
        assert(constant2(3).take(4).toList == List(3, 3, 3, 3))
        assert(from2(2).take(5).toList == List(2, 3, 4, 5, 6))
        assert(fibs2.take(5).toList == List(1, 1, 2, 3, 5))
    }

    test("Exercise 5.13") {
        assert(map(a)(_ * 2).toList == List(2, 4, 6, 8, 10))
        assert(map(e)(_ => 2) == Empty)

        assert(take(a, 2).toList == List(1, 2))
        assert(take(a, 9).toList == List(1, 2, 3, 4, 5))
        assert(take(e, 1) == Empty)

        assert(takeWhile(a)(_ < 4).toList == List(1, 2, 3))
        assert(takeWhile(e)(_ => true) == Empty)

        val a2 = Stream(0, 9, 4, 5, 7)
        assert(zipWith(a, a2)(_ + _).toList == List(1, 11, 7, 9, 12))
        assert(zipWith(a, e)(_ + _).toList == List(1, 2, 3, 4, 5))
        assert(zipWith(e, a)(_ + _).toList == List(1, 2, 3, 4, 5))
        assert(zipWith(e, e)((x: Nothing, _: Nothing) => x) == Empty)

        val a3 = Stream("a", "b", "c")
        assert(zipAll(a, a3).toList == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), Some("c")), (Some(4), None), (Some(5), None)))
        assert(zipAll(e, a3).toList == List((None, Some("a")), (None, Some("b")), (None, Some("c"))))
    }

    test("Exercise 5.14") {
        assert(startsWith(a, Stream(1, 2, 3)))
        assert(! startsWith(a, Stream(1, 2, 5)))
        assert(! startsWith(e, Stream(1, 2)))
    }

    test("Exercise 5.15") {
        assert(tails(a).map(_.toList).toList == List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5), List(3, 4, 5), List(4, 5), List(5)))
        assert(tails(e) == Empty)

        assert(hasSubSequence(a, Stream(1, 2, 3)))
        assert(hasSubSequence(a, Stream(3, 4)))
        assert(! hasSubSequence(a, Stream(5, 4)))
    }

    test("Exercise 5.16") {
        assert(scanRight(a, 2)(_ + _).toList == List(17, 16, 14, 11, 7))
    }

}
