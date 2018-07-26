import parser._

trait JSON
object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON

    /*
      JSON BNF Grammar
      JSON      := Object
      Object    := '{' KVSeq '}'
      KVSeq     := Key:Value[,KVSeq]
      Key       := String                                           --- done
      Value     := String | Boolean | Number | Array | Object       --- done
      Array     := '[' ArrSeq ']'                                   --- done
      ArrSeq    := Value[,ArrSeq]                                   --- done
      String    := '"'.+'"'                                         --- done
      Boolean   := 'true' | 'false'                                 --- done
      Number    := ???                                              --- done

      And only the value symbol need be trimed and it covers all conditions
     */
    // Exercise 9.9
    def parse[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
        import P._

         // for json parser conveninent
        // def whiteChars: Parser[String] = regex("\\s*".r)
        def whiteChars: Parser[String] = many(string(" ") | string("\n") | string("\t") | string("\r")).slice
        def braceOpen: Parser[String] = string("{")
        def braceClose: Parser[String] = string("}")
        def bracketOpen: Parser[String] = string("[")
        def bracketClose: Parser[String] = string("]")
        def comma: Parser[String] = string(",")
        def colon: Parser[String] = string(":")
        def quota: Parser[String] = string("\"")

        // trim white characters
        def trim[A](p: Parser[A]): Parser[A] = map3(whiteChars, p, whiteChars)((_, b, _) => b)

        // real syntax symbol
        def bool: Parser[JBool] = trim(map(string("true") | string("false"))({
            case "true" => JBool(true)
            case "false" => JBool(false)
        }))

        def str: Parser[JString] = trim(map3(quota, regex("[a-zA-A0-9_\\- ]+".r), quota)((_, c, _) => JString(c)))

        def number: Parser[JNumber] = trim(regex("[+-]*[1-9]+[0-9]*(.[0-9]+)?|0.[0-9]+".r) map (a => JNumber(a.toDouble)))

        def value: Parser[JSON] = bool | str | number | array | obj

        def arrseq: Parser[List[JSON]] = map2(value, many(map2(comma, value)((_, b) => b)))(_ :: _)

        def array: Parser[JArray] = trim(map3(bracketOpen, arrseq, bracketClose)((_, b, _) => JArray(b.toIndexedSeq)))

        def key: Parser[String] = str map (_.get)

        def kv: Parser[(String, JSON)] = map3(key, colon, value)((a, _, c) => (a, c))

        def kvseq: Parser[List[(String, JSON)]] = map2(kv, many(map2(comma, kv)((_, b) => b)))(_ :: _)

        def obj: Parser[JObject] = trim(map3(braceOpen, trim(kvseq), braceClose)((_, b, _) => JObject(b.toMap)))

        obj
    }
}