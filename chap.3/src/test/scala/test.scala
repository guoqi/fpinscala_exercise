import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import main._
import main.List._
import main.Tree._


@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {

    val ints = List(1, 3, 8, 4, 6, 4, 3, 8)
    val floats = List(1.0, 2.2, 3.5, 0.8)
    val strings = List("hhh", "foo", "uuuuuuuuu", "ab", "z")
    val empty = List()

    test("Exercise 3.2") {
        assert(tail(ints) == List(3, 8, 4, 6, 4, 3, 8))
        assert(tail(strings) == List("foo", "uuuuuuuuu", "ab", "z"))
        assertThrows[IndexOutOfBoundsException] { tail(empty) }
    }

    test("Exercise 3.3") {
        val t: List[Int] = setHead(ints, 10)
        assert(head(t) == 10)
        val tt = setHead(strings, "pia")
        assert(head(tt) == "pia")
        assertThrows[IndexOutOfBoundsException] {
            setHead(empty, 10)
        }
    }

    test("Exercise 3.4") {
        assert(drop(ints, 3) == List(4, 6, 4, 3, 8))
        assert(drop(strings, 4) == List("z"))
        assertThrows[IndexOutOfBoundsException] {
            drop(empty, 1)
        }
    }

    test("Exercise 3.5") {
        assert(dropWhile[Int](ints, _ %2 != 0) == List(8, 4, 6, 4, 3, 8))
        assert(dropWhile[String](strings, _.length < 4) == List("uuuuuuuuu", "ab", "z"))
        assert(dropWhile[Int](empty, _ => true) == Nil)
    }

    test("Exercise 3.6") {
        assert(init(ints) == List(1, 3, 8, 4, 6, 4, 3))
        assert(init(strings) == List("hhh", "foo", "uuuuuuuuu", "ab"))
        assert(init(empty) == Nil)
    }

    test("Exercise 3.9") {
        assert(length(ints) == 8)
        assert(length(strings) == 5)
        assert(length(empty) == 0)
    }

    test("Exercise 3.10") {
        assert(foldLeft(ints, 0)(_ + _) == 37)
        assert(foldLeft(strings, 0)(_ + _.length) == 18)
        assert(foldLeft[Int, Int](empty, 1)(_ + _) == 1)
    }

    test("Exercise 3.11") {
        assert(sum(ints) == 37)
        assert((product(floats) - 6.16).abs < 0.0000001)
    }

    test("Exercise 3.12") {
        assert(reverse(ints) == List(8, 3, 4, 6, 4, 8, 3, 1))
        assert(reverse(strings) == List("z", "ab", "uuuuuuuuu", "foo", "hhh"))
    }

    test("Exercise 3.13") {
        // TODO
    }

    test("Exercise 3.14") {
        val ints_2 = List(900, 200, 400)

        assert(append(ints, ints_2) == List(1, 3, 8, 4, 6, 4, 3, 8, 900, 200, 400))
        assert(append2(ints, ints_2) == append(ints, ints_2))
    }

    test("Exercise 3.15") {
        val nests = List(List(1, 2, 3), List(5, 4, 1), List(), List(200))

        assert(Concat(nests) == List(1, 2, 3, 5, 4, 1, 200))
        assert(Concat(empty) == Nil)
    }

    test("Exercise 3.16") {
        assert(add_by_1(ints) == List(2, 4, 9, 5, 7, 5, 4, 9))
        assert(add_by_1(empty) == Nil)
    }

    test("Exercise 3.17") {
        assert(stringify(floats) == List("1.0", "2.2", "3.5", "0.8"))
        assert(stringify(empty) == Nil)
    }

    test("Exercise 3.18") {
        assert(List.map(ints)(_ * 2) == List(2, 6, 16, 8, 12, 8, 6, 16))
        assert(List.map(floats)(_ / 2) == List(0.5, 1.1, 1.75, 0.4))
        assert(List.map[Int, Int](empty)(_ => 1) == Nil)
    }

    test("Exercise 3.19") {
        assert(filter(strings)(_.length < 3) == List("ab", "z"))
        assert(filter(floats)(_ >= 1) == List(1.0, 2.2, 3.5))

        assert(removeOddInt(ints) == List(8, 4, 6, 4, 8))
        assert(filter[Int](empty)(_ => true) == Nil)
    }

    test("Exercise 3.20") {
        val tmp = List(4, 2, 5)

        assert(flatMap(tmp)(x => List(1, x)) == List(1, 4, 1, 2, 1, 5))
        assert(flatMap[Int, List[Int]](empty)(x => List()) == Nil)
    }

    test("Exercise 3.21") {
        assert(filter2(strings)(_.length < 3) == List("ab", "z"))
        assert(filter2(floats)(_ >= 1) == List(1.0, 2.2, 3.5))
        assert(filter2[Int](empty)(_ => true) == Nil)
    }

    test("Exercise 3.22") {
        val l = List(4, 3, 100)
        val g = List(2, 9, 1, 8)
        assert(zipAdd(l, g) == List(6, 12, 101, 8))
        assert(zipAdd(l, empty) == List(4, 3, 100))
        assert(zipAdd(empty, g) == List(2, 9, 1, 8))
    }

    test("Exercise 3.23") {
        val l = List(4, 3, 100)
        val g = List(2, 9, 1, 8)

        assert(zipWith(l, g)(_ + _) == List(6, 12, 101, 8))
        assert(zipWith(l, g)(_ * _) == List(8, 27, 100, 8))
        assert(zipWith(l, empty)(_ + _) == l)
        assert(zipWith(empty, g)(_ * _) == g)
    }

    test("Exercise 3.24") {
        assert(hasSubsequence(Nil, Nil))
        assert(hasSubsequence(ints, ints))
        assert(! hasSubsequence(Nil, ints))
        assert(hasSubsequence(ints, Nil))

        val sub = List(1, 3, 8, 4, 6, 4)
        val nosub = List(1, 3, 8, 4, 6, 4, 2)
        assert(hasSubsequence(ints, sub))
        assert(! hasSubsequence(ints, nosub))
    }

    /**
      *    /\
      *  /\  3
      * 1  2
      */
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val tree2 = Leaf(1)

    test("Exercise 3.25") {
        assert(size(tree) == 3)
        assert(size(tree2) == 1)
    }

    test("Exercise 3.26") {
        assert(maximum(tree) == 3)
        assert(maximum(tree2) == 1)
    }

    test("Exercise 3.27") {
        assert(depth(tree) == 3)
        assert(depth(tree2) == 1)
    }

    test("Exercise 3.28") {
        assert(Tree.map(tree)(_ * 2) == Branch(Branch(Leaf(2), Leaf(4)), Leaf(6)))
        assert(Tree.map(tree2)(_ * 2) == Leaf(2))
    }

    test("Exercise 3.29") {
        assert(fold(tree, 0)(_ + _, _ * _) == 6)
        assert(fold(tree2, 0)(_ + _, _ * _) == 1)

        assert(size2(tree) == size(tree))
        assert(size2(tree2) == size(tree2))

        assert(maximum2(tree) == maximum(tree))
        assert(maximum2(tree2) == maximum(tree2))

        assert(depth2(tree) == depth(tree))
        assert(depth2(tree2) == depth(tree2))

        assert(Tree.map2(tree)(_ * 2) == Tree.map(tree)(_ * 2))
        assert(Tree.map2(tree2)(_ * 2) == Tree.map(tree2)(_ * 2))
    }
}
