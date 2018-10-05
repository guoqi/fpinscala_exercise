import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMonad extends FunSuite {
    import Monad._

    test("Exercise 11.1") {
        assert(optionMonad.flatMap(Some(1))(a => Some(a * 2)) == Some(1).flatMap(a => Some(a * 2)))
        assert(optionMonad.flatMap(None)(_ => Some(1)).isEmpty)
        assert(optionMonad.unit(123).contains(123))

        val s = Stream(1, 2, 3, 4)
        assert(streamMonad.flatMap(s)(a => Stream(a * 2)) == Stream(2, 4, 6, 8))
        assert(streamMonad.unit(1) == Stream(1))

        val l = List(1, 2, 3, 5)
        assert(listMonad.flatMap(l)(a => (1 to a).toList) == List(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 5))
        assert(listMonad.unit(123) == List(123))
    }

    test("Exercise 11.3") {
        val l = List(List(4), List(6, 7, 8, 9, 10))
        val l2 = List(List(1,2,3), List(1,2,3))
        assert(listMonad.sequence(l) == List(List(4,6), List(4,7), List(4,8), List(4,9), List(4,10)))
        assert(listMonad.sequence(l2) == List(List(1,1), List(1,2), List(1,3), List(2,1), List(2,2), List(2,3), List(3,1), List(3,2), List(3,3)))

        val l3 = List(Some(1), None, Some(2), None, None)
        val l4 = List(None, None, None)
        val l5 = List(Some(1), Some(2), Some(3))
        assert(optionMonad.sequence(l3).isEmpty)
        assert(optionMonad.sequence(l4).isEmpty)
        assert(optionMonad.sequence(l5).contains(List(1,2,3)))

        val l6 = List(1, 2, 3, 4)
        assert(listMonad.traverse(l6)(a => List(a * 2)) == List(List(2, 4, 6, 8)))
        assert(optionMonad.traverse(l5)(_ => Some(1.0)).contains(List(1.0, 1.0, 1.0)))
    }

    test("Exercise 11.4") {
        assert(listMonad.replicateM(2, List(1,2)) == List(List(1,1), List(1,2), List(2,1), List(2,2)))
        assert(optionMonad.replicateM(3, Some(1)).contains(List(1, 1, 1)))
        assert(optionMonad.replicateM(3, None).isEmpty)
    }

    test("Exercise 11.6") {
        val l = List(1, 2, 3, 4)
        assert(listMonad.filterM(l)(a => if (a % 2 == 0) List(true) else List(false)) == List(List(2, 4)))
    }

    test("Exercise 11.12") {
        val l = List(List(4), List(6, 7, 8, 9, 10))
        assert(listMonad.join(l) == List(4, 6, 7, 8, 9, 10))
    }

}
