import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMonad extends FunSuite {
    import Monad._
    test("listMonad") {
        val l = List(List(1, 2, 3), List(4, 5), List(6, 7, 8, 9, 10))
        val l2 = List(List(1,2,3), List(1,2,3))
        println(listMonad.sequence(l2))

        val l3 = List(Some(1), None, Some(2), None, None)
        val l4 = List(None, None, None)
        val l5 = List(Some(1), Some(2), Some(3))
        println(optionMonad.sequence(l3))
        println(optionMonad.sequence(l4))
        println(optionMonad.sequence(l5))
    }
}
