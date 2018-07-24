import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import parser._

@RunWith(classOf[JUnitRunner])
class TestMyParser extends FunSuite {
    import MyParsers._

    test("string") {
        val strParser = scope("dont match abc")(string("abc"))
        val err = (Location("abab", 0), "dont match abc")
        assert(MyParsers.run(strParser)("abcdef") == Right("abc"))
        assert(MyParsers.run(strParser)("abcabc") == Right("abc"))
        assert(MyParsers.run(strParser)("abab") == Left(ParseError(List(err))))
    }

    test("regex") {
        val p = scope("dont match regex expression")(regex("\"[a-zA-Z0-9_\\- ]+\"".r))
        val err = (Location("\"hhhhh", 0), "dont match regex expression")
        val err1 = (Location("\"\"", 0), "dont match regex expression")
        val err2 = (Location("1\"abcedf\"", 0), "dont match regex expression")
        assert(MyParsers.run(p)("\"hhhhhh-jjjj_piapia \"") == Right("\"hhhhhh-jjjj_piapia \""))
        assert(MyParsers.run(p)("\"hhhhh") == Left(ParseError(List(err))))
        assert(MyParsers.run(p)("\"\"") == Left(ParseError(List(err1))))
        assert(MyParsers.run(p)("1\"abcedf\"") == Left(ParseError(List(err2))))
    }

    test("succeed") {
        val p = scope("hhhh")(MyParsers.succeed(1.1))
        assert(MyParsers.run(p)("") == Right(1.1))
        assert(MyParsers.run(p)("abc") == Right(1.1))
        assert(MyParsers.run(p)("123123") == Right(1.1))
        assert(MyParsers.run(p.slice)("hhhhh") == Right(""))
    }

    test("or") {
        val p1 = scope("dont match abc")(string("abc"))
        val p2 = scope("dont match aaa")(string("aaa"))
        assert(MyParsers.run(p1 | p2)("abcabc") == Right("abc"))
        assert(MyParsers.run(p1 | p2)("aaabbb") == Right("aaa"))
        assert(MyParsers.run(p1 | p2)("hhhhhhh") == Left(ParseError(List((Location("hhhhhhh", 0), "dont match aaa")))))
    }

    test("flatMap") {
        val p = scope("dont match digit")(regex("\\d+".r))
        val p2 = p.flatMap[Int](s => MyParsers.succeed(s.toInt))
        val p3 = p.flatMap[Int](s => MyParsers.succeed(s.length))
        val err = (Location("abcdef", 0), "dont match digit")
        assert(MyParsers.run(p2)("1231231231") == Right(1231231231))
        assert(MyParsers.run(p3)("1231231231") == Right(10))
        assert(MyParsers.run(p2)("abcdef") == Left(ParseError(List(err))))
    }

    test("silce") {
        val p = scope("dont match word")(regex("\\w+".r))
        val p2 = p.flatMap(s => MyParsers.succeed(s.length))
        val err = (Location("-123", 0), "dont match word")
        assert(MyParsers.run(p.slice)("abcdef") == Right("abcdef"))
        assert(MyParsers.run(p2.slice)("hhhhh") == Right("hhhhh"))
        assert(MyParsers.run(p.slice)("123-456") == Right("123"))
        assert(MyParsers.run(p.slice)("-123") == Left(ParseError(List(err))))
    }

    test("map") {
        val p = scope("dont match number")(regex("[+-]?\\d+".r))
        val p2 = p map (_.toInt * 2)
        val err = (Location("abc", 0), "dont match number")
        assert(MyParsers.run(p2)("123") == Right(246))
        assert(MyParsers.run(p2)("-22") == Right(-44))
        assert(MyParsers.run(p2)("+1") == Right(2))
        assert(MyParsers.run(p2)("abc") == Left(ParseError(List(err))))
    }

    test("many") {
        val p = many(string("abc") | string("bac"))
        assert(MyParsers.run(p)("abcabcabchhh") == Right(List("abc", "abc", "abc")))
        assert(MyParsers.run(p)("bacbacbac") == Right(List("bac", "bac", "bac")))
        assert(MyParsers.run(p)("abcbacabc") == Right(List("abc", "bac", "abc")))
        assert(MyParsers.run(p)("abababab") == Right(Nil))
        assert(MyParsers.run(p)("") == Right(Nil))
    }

    test("product") {
        val p = product(string("aaa"), string("bbb"))
        assert(MyParsers.run(p)("aaabbbhhh") == Right(("aaa", "bbb")))
        assert(MyParsers.run(p)("aabbaa") == Left(ParseError(Nil)))
        assert(MyParsers.run(p)("aaabbc") == Left(ParseError(Nil)))
    }

    test("map2") {
        val p = map2(string("123"), string("89"))(_.toInt + _.toInt)
        assert(MyParsers.run(p)("12389") == Right(212))
        assert(MyParsers.run(p)("abc") == Left(ParseError(Nil)))
    }

    test("map3") {
        val p = map3(string("\""), regex("\\w+".r), string("\""))((_, b, _) => b)
        assert(MyParsers.run(p)("\"hello\"") == Right("hello"))
        assert(MyParsers.run(p)("\"h w\"") == Left(ParseError(Nil)))
    }

    test("many1") {
        val p = many1(string("abc") | string("bac"))
        assert(MyParsers.run(p)("abc") == Right(List("abc")))
        assert(MyParsers.run(p)("abcbac") == Right(List("abc", "bac")))
        assert(MyParsers.run(p)("aaa") == Left(ParseError(Nil)))
    }

    test("listOfN") {
        val p = listOfN(3, string("abc"))
        assert(MyParsers.run(p)("abcabcabc") == Right(List("abc", "abc", "abc")))
        assert(MyParsers.run(p)("abcabc") == Left(ParseError(Nil)))
    }

    test("sensParser") {
        assert(MyParsers.run(sensParser)("0") == Right(Nil))
        assert(MyParsers.run(sensParser)("1a") == Right(List("a")))
        assert(MyParsers.run(sensParser)("4aaaa") == Right(List("a", "a", "a", "a")))
    }

    import JSON._

    private val p = JSON.parse(MyParsers)

    test("trim") {
        val whiteCharList = many(string(" ") | string("\n") | string("\t") | string("\r"))
        val whiteChars = whiteCharList.slice
        def trim[A](p: MyParser[A]): MyParser[A] = map3[String, A, String, A](whiteChars, p, whiteChars)((_, b, _) => b)

        val p = trim(string("hhh"))
        val p1 = product(whiteChars, string("hhh"))

        assert(MyParsers.run(whiteCharList)("\n\t\r ") == Right(List("\n", "\t", "\r", " ")))
        assert(MyParsers.run(whiteChars)("\n\t\r hhh") == Right("\n\t\r "))
        assert(MyParsers.run(p1)("     hhh") == Right(("     ", "hhh")))
        assert(MyParsers.run(p)("\n\t\rhhhh   ") == Right("hhh"))
    }

    test("JSON - JString") {
        val json1 = " { \"a\" : \"hello\" , \"b\" :\"world\" } "
        val json2 = JObject(Map(
            "a"->JString("hello"),
            "b"->JString("world")
        ))
        assert(MyParsers.run(p)(json1) == Right(json2))
    }

    test("JSON - JNumber") {
        val json1 = "{    \"a\": 123, \"b\": 456.34, \"c\": 0.123 }"
        val json2 = JObject(Map(
            "a" -> JNumber(123),
            "b" -> JNumber(456.34),
            "c" -> JNumber(0.123)
        ))
        assert(MyParsers.run(p)(json1) == Right(json2))
    }

    test("JSON - whole") {
        val json1 = "\t{  \"a\": 123,  \"b\": true,   \"c\"  :  \"hhh\"  ,  \"d\":  [1,true,  \"123\", {\"a\":123} ],\"e\": {\"a\":\"hhh\"}}\n"
        val json2 = JObject(Map(
            "a"->JNumber(123),
            "b"->JBool(true),
            "c"->JString("hhh"),
            "d"->JArray(List(JNumber(1), JBool(true), JString("123"), JObject(Map("a"->JNumber(123)))).toIndexedSeq),
            "e"->JObject(Map(
                "a"->JString("hhh")
            ))
        ))
        val j = MyParsers.run(p)(json1)
        assert(j == Right(json2))
    }
}
