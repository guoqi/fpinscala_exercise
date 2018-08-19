import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import check._
import check.Prop._

@RunWith(classOf[JUnitRunner])
class CheckTest extends FunSuite {
    private val l = Gen.listOf(Gen.choose(0, 100))

    test("Exercise 8.1") {
        val prop1 = forAll(l)(ls => ls.sum == ls.reverse.sum)
        prop1.check()
    }

    test("Exercise 8.2") {
        val prop = forAll(l)(ls => ls.forall(_ <= ls.max))
        val prop1 = forAll(l)(ls => ls.forall(_ < ls.max))
        prop.check()
        prop1.check()
    }
}
