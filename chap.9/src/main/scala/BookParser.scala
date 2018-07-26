import parser._
import scala.util.matching.Regex

package object bookParser {

    trait Result[+A] {
        // helper function
        def mapError (f: ParseError => ParseError): Result[A] = this match {
            case Failure(e, isCommited) => Failure(f(e), isCommited)
            case _ => this
        }

        def uncommit: Result[A] = this match {
            case Failure(e, true) => Failure(e, isCommited = false)
            case _ => this
        }

        def addCommit (isCommited: Boolean): Result[A] = this match {
            case Failure(e, c) => Failure(e, c || isCommited)
            case _ => this
        }

        def advanceSuccess (n: Int): Result[A] = this match {
            case Success(a, m) => Success(a, m + n)
            case _ => this
        }
    }

    case class Success[+A] (get: A, charConsumed: Int) extends Result[A]

    case class Failure (get: ParseError, isCommited: Boolean) extends Result[Nothing]


    type BookParser[+ A] = Location => Result[A]

    object BookParsers extends Parsers[BookParser] {

        override def run[A] (p: BookParser[A])(input: String): Either[ParseError, A] = p(Location(input)) match {
            case Success(a, _) => Right(a)
            case Failure(e, _) => Left(e)
        }

        override def string (s: String): BookParser[String] =
            loc => if (loc.input.startsWith(s, loc.offset)) Success(s, s.length) else Failure(loc.toError("Expected: " + s), isCommited = false)

        override def regex (r: Regex): BookParser[String] = loc => r.findPrefixOf(loc.input.substring(loc.offset)) match {
            case None => Failure(loc.toError("Regex match error: " + r.toString()), isCommited = false)
            case Some(x) => Success(x, x.length)
        }

        override def succeed[A] (a: A): BookParser[A] = _ => Success(a, 0)

        override def slice[A] (p: BookParser[A]): BookParser[String] = loc => p(loc) match {
            case Success(_, c) => Success(loc.input.substring(loc.offset, loc.offset + c), c)
            case e@Failure(_, _) => e
        }

        override def flatMap[A, B] (p: BookParser[A])(f: A => BookParser[B]): BookParser[B] = loc => p(loc) match {
            case Success(a, n) => f(a)(loc.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
            case e@Failure(_, _) => e
        }

        override def or[A] (p1: BookParser[A], p2: => BookParser[A]): BookParser[A] = loc => p1(loc) match {
            case Failure(_, _) => p2(loc)
            case r => r
        }

        override def label[A] (msg: String)(p: BookParser[A]): BookParser[A] = loc => p(loc) mapError (_.label(msg))

        override def scope[A] (msg: String)(p: BookParser[A]): BookParser[A] = loc => p(loc) mapError (_.push(loc, msg))

        override def attempt[A] (p: BookParser[A]): BookParser[A] = loc => p(loc).uncommit

    }

}