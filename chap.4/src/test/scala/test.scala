import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Option._
import Either._


@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
    test("Exercise 4.1") {
        assert(None.map(x => x) == None)
        assert(None.flatMap(_ => None) == None)
        assert(None.getOrElse(1) == 1)
        assert(None.orElse(Some(1)) == Some(1))
        assert(None.filter(_ => true) == None)

        val a = Some(1)
        assert(a.map(x => x.toString) == Some("1"))
        assert(a.flatMap(x => Some(x.toString)) == Some("1"))
        assert(a.getOrElse(2) == 1)
        assert(a.orElse(Some(2)) == Some(1))
        assert(a.filter(x => x == 1) == Some(1))
        assert(a.filter(x => x != 1) == None)
    }

    test("Exercise 4.2") {
        val as: List[Double] = List(1, 2, 3, 4)
        assert(variance(as) == Some(1.25))
        assert(variance(List()) == None)
    }

    test("Exercise 4.3") {
        val a = Some("123")
        val b = Some(456)

        assert(map2(a, b)((x, y) => x + y.toString) == Some("123456"))
        assert(map2(a, None)((x, y) => x + y.toString) == None)
        assert(map2(None, b)((x, y) => y.toString) == None)
        assert(map2(None, None)((x, _) => x) == None)
    }

    test("Exercise 4.4") {
        val as = List(Some(1), Some(2), Some(3), Some(4))
        val as2 = List(Some(1), None, Some(2))

        assert(Option.sequence(as) == Some(List(1, 2, 3, 4)))
        assert(Option.sequence(as2) == None)
    }

    test("Exercise 4.5") {
        val as = List(Some(1), Some(2), Some(3), Some(4))
        val as2 = List(Some(1), None, Some(2))

        assert(sequence2(as) == Some(List(1, 2, 3, 4)))
        assert(sequence2(as2) == None)
    }

    test("Exercise 4.6") {
        val l = Left("hhhh")
        val r = Right(1)

        assert(l.map(x => x) == l)
        assert(l.flatMap(x => x) == l)
        assert(l.orElse(r) == r)
        assert(l.map2(r)((_, a) => a) == l)
        assert(l.map2(l)((_, a) => a) == l)

        assert(r.map(x => x + 1) == Right(2))
        assert(r.flatMap(x => Right(x + 1)) == Right(2))
        assert(r.orElse(l) == r)
        assert(r.map2(l)((a, _) => a) == l)
        assert(r.map2(r)((a, b) => a + b) == Right(2))
    }

    test("Exercise 4.7") {
        val as = List(Right(1), Right(2), Left("piapia"), Right(3), Left("hhh"))
        val as2 = List(Right(1), Right(2), Right(3))

        assert(Either.sequence(as) == Left("piapia"))
        assert(Either.sequence(as2) == Right(List(1, 2, 3)))
    }
}