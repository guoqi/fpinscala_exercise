import parser._

import scala.util.matching.Regex

case class Cursor(input: String, offset: Int) {
    lazy val start: String = input.substring(offset)
}

case class Result[+A](m: String, value: A)

case class MyParser[+A](commit: Cursor => Either[ParseError, (Cursor, Result[A])]) extends Parser[A]

object MyParsers extends Parsers[MyParser] {

    override def run[A] (p: MyParser[A])(input: String): Either[ParseError, A] = p.commit(Cursor(input, 0)) match {
        case Left(e) => Left(e)
        case Right((_, r)) => Right(r.value)
    }

    override def string (s: String): MyParser[String] = MyParser(c => {
        if (c.start.startsWith(s)) Right(Cursor(c.input, c.offset + s.length), Result(s, s)) else Left(ParseError(List()))
    })

    override def or[A] (p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = MyParser(c => p1.commit(c) match {
        case Left(_) => p2.commit(c)
        case Right((c2, r)) => Right((c2, r))
    })

    override def slice[A] (p: MyParser[A]): MyParser[String] = MyParser(c => p.commit(c) match {
        case Left(e) => Left(e)
        case Right((c2, r)) => Right((c2, Result(r.m, r.m)))
    })

    override def succeed[A] (a: A): MyParser[A] = MyParser(c => Right((c, Result("", a))))

    override def flatMap[A, B] (p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = MyParser(c => p.commit(c) match {
        case Left(e) => Left(e)
        case Right((c2, r)) => f(r.value).commit(c2) match {
            case Left(e) => Left(e)
            case Right((c3, r2)) => Right((c3, Result(r.m + r2.m, r2.value)))
        }
    })

    override def regex (r: Regex): MyParser[String] = MyParser(c => r.findPrefixOf(c.start) match {
        case None => Left(ParseError(Nil))
        case Some(x) => Right(Cursor(c.input, c.offset + x.length), Result(x, x))
    })

    override def label[A] (msg: String)(p: MyParser[A]): MyParser[A] = MyParser(c => p.commit(c) match {
        case Left(e) => Left(ParseError(List((Location(c.input, c.offset), msg))))
        case Right(x) => Right(x)
    })

    override def scope[A] (msg: String)(p: MyParser[A]): MyParser[A] = MyParser(c => p.commit(c) match {
        case Left(e) => Left(ParseError((Location(c.input, c.offset), msg) :: e.stack))
        case Right(x) => Right(x)
    })

    // override def errorLocation (e: ParseError): Location = ???

    // override def errorMessage (e: ParseError): String = ???

    // override def attempt[A] (p: MyParser[A]): MyParser[A] = ???

}
