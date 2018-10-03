import check.Gen
import check.Prop
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMonoid extends FunSuite {
    import Exercise._
    def testMonoidProp[A](m: Monoid[A]): Unit = {
        val g = Gen.unit(m.zero)
        monoidLaws(m, g).check()
    }

    test("Exercise 10.1") {
        testMonoidProp(intAddition)
        testMonoidProp(intMultiplication)
        testMonoidProp(booleanOr)
        testMonoidProp(booleanAnd)
    }

    test("Exercise 10.2") {
        testMonoidProp(optionMonoid[String])
    }

    test("Exercise 10.3") {
        val m = endoMonoid[Int]
        def f1(a: Int): Int = a + 1
        def f2(a: Int): Int = a * 3
        def f3(a: Int): Int = if (a <= 1) a else a * f3(a - 1)

        val cf1 = m.op(m.op(f1, f2), f3)
        val cf2 = m.op(f1, m.op(f2, f3))
        val g = Gen.choose(0, 5)

        Prop.forAll(g)(a => cf1(a) == cf2(a)).check()
        Prop.forAll(g)(a => m.op(m.zero, f1)(a) == m.op(f1, m.zero)(a) && m.op(m.zero, f1)(a) == f1(a)).check()
        Prop.forAll(g)(a => m.op(m.zero, f2)(a) == m.op(f2, m.zero)(a) && m.op(m.zero, f2)(a) == f2(a)).check()
        Prop.forAll(g)(a => m.op(m.zero, f3)(a) == m.op(f3, m.zero)(a) && m.op(m.zero, f3)(a) == f3(a)).check()
    }

    test("Exercise 10.4") {
        testMonoidProp(intAddition)
    }

    test("Exercise 10.5") {
        val lg = Gen.listOf(Gen.choose(1, 1000))
        Prop.forAll(lg)(l => foldMap(l, intAddition)(a => a) == l.sum).check()
        Prop.forAll(lg)(l => foldMap(l, intMultiplication)(a => a) == l.product).check()
    }

    test("Exercise 10.6") {
        val lg = Gen.listOf(Gen.choose(1, 1000))
        Prop.forAll(lg)(l => foldLeft(l)(1)(_ + _) == l.sum + 1).check()
        Prop.forAll(lg)(l => foldRight(l)(1)(_ + _) == l.sum + 1).check()
    }

    test("Exercise 10.7") {
        val seq = IndexedSeq(1, 3, 2, 9, 4, 88, 12, 0, 9)

        assert(foldMapV(seq, intAddition)(a => a) == seq.sum)
    }

    test("Exercise 10.8") {
        // TODO
    }

    test("Exercise 10.9") {
        val seq1 = IndexedSeq(1, 3, 2, 9, 4, 88, 12, 0, 9)
        val seq2 = IndexedSeq(1, 2, 3, 4, 5, 10, 234)
        // TODO
        //assert(! isOrderd(seq1)) // seq1 is unordered
        //assert(isOrderd(seq2)) // and seq2 is ordered
    }

    test("Exercise 10.10 && Exercise 10.11") {
        val s1 = "abcdefghij"
        val s2 = "hello world,welcome to china"
        val s3 = "Perhaps the most important thing we can do for C++ at this point in its evolution is to make sure we preserve its core strengths while also directing its evolution in ways that make it simpler to use. That is my own opinion at least, so this talk starts with a perspective question: What “is C++,” really? The language continues to evolve and change; as it does so, how can we be sure we’re picking C++ evolutionary improvements that not only don’t lose its “C++-ic” qualities, but make it a better C++ than ever?\n\nAt recent CppCons, I’ve spoken about several of my own personal C++ evolution efforts and experiments, and why I think they’re potentially important directions to explore for making C++ both more powerful and also simpler to use. The bulk of the talk is updates on two of these:"
        assert(wordCount(s1) == 1)
        assert(wordCount(s2) == 5)
        assert(wordCount(s3) == 142)
    }

    test("Exercise 10.12") {
        // Test flist
        val lg = Gen.listOf(Gen.choose(1, 1000))
        Prop.forAll(lg)(l => flist.foldMap(l)(a => a)(intAddition) == l.sum).check()
        Prop.forAll(lg)(l => flist.foldMap(l)(a => a)(intMultiplication) == l.product).check()
        Prop.forAll(lg)(l => flist.foldLeft(l)(1)(_ + _) == l.sum + 1).check()
        Prop.forAll(lg)(l => flist.foldRight(l)(1)(_ + _) == l.sum + 1).check()

        // Test findexseq
        val sg = lg.map(l => l.toIndexedSeq)
        Prop.forAll(sg)(l => findexedseq.foldMap(l)(a => a)(intAddition) == l.sum).check()
        Prop.forAll(sg)(l => findexedseq.foldMap(l)(a => a)(intMultiplication) == l.product).check()
        Prop.forAll(sg)(l => findexedseq.foldLeft(l)(1)(_ + _) == l.sum + 1).check()
        Prop.forAll(sg)(l => findexedseq.foldRight(l)(1)(_ + _) == l.sum + 1).check()

        // Test fstream
        val ss = lg.map(l => l.toStream)
        Prop.forAll(ss)(l => fstream.foldMap(l)(a => a)(intAddition) == l.sum).check()
        Prop.forAll(ss)(l => fstream.foldMap(l)(a => a)(intMultiplication) == l.product).check()
        Prop.forAll(ss)(l => fstream.foldLeft(l)(1)(_ + _) == l.sum + 1).check()
        Prop.forAll(ss)(l => fstream.foldRight(l)(1)(_ + _) == l.sum + 1).check()
    }

    test("Exercise 10.13") {
        /**
          *         /   \
          *       1 \    /  \
          *         /\   10  4
          *        2  /\
          *          9  111
          */
        val tree = Branch(
            Branch(
                Leaf(1),
                Branch(
                    Leaf(2),
                    Branch(
                        Leaf(9),
                        Leaf(111)
                    )
                )
            ),
            Branch(Leaf(10), Leaf(4)))
        val l = List(1, 2, 9, 111, 10, 4)

        assert(ftree.foldLeft(tree)(0)(_ + _) == ftree.foldRight(tree)(0)(_ + _))
        assert(ftree.foldLeft(tree)(0)(_ + _) == ftree.foldMap(tree)(a => a)(intAddition))
        assert(ftree.foldLeft(tree)(0)(_ + _) == l.sum)

        assert(ftree.foldLeft[Int, List[Int]](tree)(Nil)((l, a) => a::l) == l.reverse)
        assert(ftree.foldRight[Int, List[Int]](tree)(Nil)((a, l) => a::l) == l)
    }

    test("Exercise 10.14") {
        assert(foption.foldLeft(Some(123))(1)(_ + _) == 124)
        assert(foption.foldLeft[Int, Int](None)(1)(_ + _) == 1)
        assert(foption.foldRight(Some(123))(1)(_ + _) == 124)
        assert(foption.foldRight[Int, Int](None)(1)(_ + _) == 1)
        assert(foption.foldMap(Some(123))(a => a)(intAddition) == 123)
        assert(foption.foldMap(None)(a => a)(intAddition) == 0)
    }

    test("Exercise 10.15") {
        assert(flist.toList(List(404, 945)) == List(404, 945))
        assert(findexedseq.toList(Vector(404, 945)) == List(404, 945))
        val lg = Gen.listOf(Gen.choose(1, 1000))
        val sg = lg.map(l => l.toIndexedSeq)
        Prop.forAll(sg)(l => findexedseq.toList(l) == l.toList).check()

        val tree = Branch(
            Branch(
                Leaf(1),
                Branch(
                    Leaf(2),
                    Branch(
                        Leaf(9),
                        Leaf(111)
                    )
                )
            ),
            Branch(Leaf(10), Leaf(4)))
        val l = List(1, 2, 9, 111, 10, 4)
        assert(ftree.toList(tree) == l)
    }

    test("Exercise 10.16") {
        val ag = Gen.choose(1, 1000)
        val bg = Gen.choose(1, 1000)
        val abg = for (a <- ag; b <- bg) yield (a, b)

        monoidLaws(productMonoid(intAddition, intMultiplication), abg).check()
    }

    test("Exercise 10.17") {
        // TODO
    }

    test("Exercise 10.18") {
        val v = Vector("a", "rose", "is", "a", "rose")
        assert(bag(v) == Map("a" -> 2, "rose" -> 2, "is" -> 1))
    }
}
