import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import parser._

@RunWith(classOf[JUnitRunner])
class TestBookParser extends FunSuite {
    import bookParser._
    import bookParser.BookParsers._

    type MyParser[+A] = BookParser[A]
    private val MyParsers = BookParsers

    test("string") {
        val strParser = scope("dont match abc")(string("abc"))
        val err = (Location("abab", 0), "dont match abc")
        val err1 = (Location("abab", 0), "Expected: abc")
        assert(MyParsers.run(strParser)("abcdef") == Right("abc"))
        assert(MyParsers.run(strParser)("abcabc") == Right("abc"))
        assert(MyParsers.run(strParser)("abab") == Left(ParseError(List(err, err1))))
    }

    test("regex") {
        val p = scope("dont match regex expression")(regex("\"[a-zA-Z0-9_\\- ]+\"".r))
        val err = (Location("\"hhhhh", 0), "dont match regex expression")
        val err1 = (Location("\"\"", 0), "dont match regex expression")
        val err2 = (Location("1\"abcedf\"", 0), "dont match regex expression")
        val err3 = (Location("\"hhhhh", 0), "Regex match error: \"[a-zA-Z0-9_\\- ]+\"")
        val err4 = (Location("1\"abcedf\"", 0), "Regex match error: \"[a-zA-Z0-9_\\- ]+\"")
        val err5 = (Location("\"\"", 0), "Regex match error: \"[a-zA-Z0-9_\\- ]+\"")
        assert(MyParsers.run(p)("\"hhhhhh-jjjj_piapia \"") == Right("\"hhhhhh-jjjj_piapia \""))
        assert(MyParsers.run(p)("\"hhhhh") == Left(ParseError(List(err, err3))))
        assert(MyParsers.run(p)("\"\"") == Left(ParseError(List(err1, err5))))
        assert(MyParsers.run(p)("1\"abcedf\"") == Left(ParseError(List(err2, err4))))
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
        val err = (Location("hhhhhhh", 0), "dont match aaa")
        val err1 = (Location("hhhhhhh", 0), "Expected: aaa")
        assert(MyParsers.run(p1 | p2)("abcabc") == Right("abc"))
        assert(MyParsers.run(p1 | p2)("aaabbb") == Right("aaa"))
        assert(MyParsers.run(p1 | p2)("hhhhhhh") == Left(ParseError(List(err, err1))))
    }

    test("flatMap") {
        val p = scope("dont match digit")(regex("\\d+".r))
        val p2 = p.flatMap[Int](s => MyParsers.succeed(s.toInt))
        val p3 = p.flatMap[Int](s => MyParsers.succeed(s.length))
        val err = (Location("abcdef", 0), "dont match digit")
        val err1 = (Location("abcdef", 0), "Regex match error: \\d+")
        assert(MyParsers.run(p2)("1231231231") == Right(1231231231))
        assert(MyParsers.run(p3)("1231231231") == Right(10))
        assert(MyParsers.run(p2)("abcdef") == Left(ParseError(List(err, err1))))
    }

    test("silce") {
        val p = scope("dont match word")(regex("\\w+".r))
        val p2 = p.flatMap(s => MyParsers.succeed(s.length))
        val err = (Location("-123", 0), "dont match word")
        val err1 = (Location("-123", 0), "Regex match error: \\w+")
        assert(MyParsers.run(p.slice)("abcdef") == Right("abcdef"))
        assert(MyParsers.run(p2.slice)("hhhhh") == Right("hhhhh"))
        assert(MyParsers.run(p.slice)("123-456") == Right("123"))
        assert(MyParsers.run(p.slice)("-123") == Left(ParseError(List(err, err1))))
    }

    test("map") {
        val p = scope("dont match number")(regex("[+-]?\\d+".r))
        val p2 = p map (_.toInt * 2)
        val err = (Location("abc", 0), "dont match number")
        val err1 = (Location("abc", 0), "Regex match error: [+-]?\\d+")
        assert(MyParsers.run(p2)("123") == Right(246))
        assert(MyParsers.run(p2)("-22") == Right(-44))
        assert(MyParsers.run(p2)("+1") == Right(2))
        assert(MyParsers.run(p2)("abc") == Left(ParseError(List(err, err1))))
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
        val err = (Location("aabbaa", 0), "Expected: aaa")
        val err1 = (Location("aaabbc", 3), "Expected: bbb")
        assert(MyParsers.run(p)("aaabbbhhh") == Right(("aaa", "bbb")))
        assert(MyParsers.run(p)("aabbaa") == Left(ParseError(List(err))))
        assert(MyParsers.run(p)("aaabbc") == Left(ParseError(List(err1))))
    }

    test("map2") {
        val p = map2(string("123"), string("89"))(_.toInt + _.toInt)
        val err = (Location("abc", 0), "Expected: 123")
        val err1 = (Location("12345", 3), "Expected: 89")
        assert(MyParsers.run(p)("12389") == Right(212))
        assert(MyParsers.run(p)("abc") == Left(ParseError(List(err))))
        assert(MyParsers.run(p)("12345") == Left(ParseError(List(err1))))
    }

    test("map3") {
        val p = map3(string("\""), regex("\\w+".r), string("\""))((_, b, _) => b)
        val err = (Location("\"h w\"", 2), "Expected: \"")
        assert(MyParsers.run(p)("\"hello\"") == Right("hello"))
        assert(MyParsers.run(p)("\"h w\"") == Left(ParseError(List(err))))
    }

    test("many1") {
        val p = many1(string("abc") | string("bac"))
        val err = (Location("aaa", 0), "Expected: bac")
        assert(MyParsers.run(p)("abc") == Right(List("abc")))
        assert(MyParsers.run(p)("abcbac") == Right(List("abc", "bac")))
        assert(MyParsers.run(p)("aaa") == Left(ParseError(List(err))))
    }

    test("listOfN") {
        val p = listOfN(3, string("abc"))
        val err = (Location("abcabc", 6), "Expected: abc")
        assert(MyParsers.run(p)("abcabcabc") == Right(List("abc", "abc", "abc")))
        assert(MyParsers.run(p)("abcabc") == Left(ParseError(List(err))))
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

    test("JSON - reportError") {
        val json1 = "\t\t{ \"a\": \"jjjj\", \"b\": abc } "
        MyParsers.run(p)(json1) match {
            case Left(e) => println(reportError(e))
            case r => r
        }
    }
}
