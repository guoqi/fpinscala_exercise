package parser

import scala.util.matching.Regex

trait Parser[+A]

case class ParseError(stack: List[(Location, String)]) {
    // helper function
    def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

    def latest: Option[(Location, String)] = stack.lastOption

    def latestLoc: Option[Location] = latest map (_._1)

    def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
}

case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset).count(_ == '\n') + 1 // line count from 1
    lazy val col: Int = input.slice(0, offset).lastIndexOf('\n') match {
        case -1 => offset + 1   // only one line or at the first line
        case lineStart => offset - lineStart
    }

    // helper function
    def toError(msg: String): ParseError = ParseError(List((this, msg)))

    def advanceBy(n: Int): Location = copy(offset = offset + n)
}

trait Parsers[Parser[+_]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    // match c
    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

    // Is this really primitive? TODO
    // In fact it isn't primitive because the implementation of map depends on this and this is be implemented by map which will cause a infinite loop
    // It is necessary to make it primitive
    // def succeed[A](a: A): Parser[A] = string("") map (_ => a)
    def succeed[A](a: A): Parser[A]

    // match s or convert String to Parser[String] implicitly
    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

    // implicit def asStringParser[A](a: A)(f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    // match p1 or match p2
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    // match zero or more elements
    // Exercise 9.3
    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(Nil)

    // as the name shows
    // Exercise 9.8
    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

    // match p and return the matchedd string
    def slice[A](p: Parser[A]): Parser[String]

    // match p and followed by p2
    // Exercise 9.2: Laws TODO
    // Exercise 9.7
    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => flatMap(p2)(b => succeed((a, b))))

    // as the name shows
    // Exercise 9.1
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(f.tupled(_))

    def map3[A, B, C, D](p: Parser[A], p2: => Parser[B], p3: => Parser[C])(f: (A, B, C) => D): Parser[D] = flatMap(p)(a => map2(p2, p3)(f(a, _, _)))

    // Exercise 9.8
    def map2_2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for(a <- p; b <- p2) yield f(a, b)

    // match one or more elements
    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

    // Exercise 9.4
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = map2(if (n <= 0) succeed(Nil) else map(p)(List(_)), if(n <= 0) succeed(Nil) else listOfN(n-1, p))(_ ::: _)

    // Exercise 9.5 TODO

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    implicit def regex(r: Regex): Parser[String]

    // Exercise 9.6
    def sensParser: Parser[List[String]] = regex("[0-9]".r) flatMap (a => listOfN(a.toInt, string("a")))

    // convenient operators
    case class ParserOps[A](p: Parser[A]) {
        def | [B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
        def many: Parser[List[A]] = self.many(p)
        def slice: Parser[String] = self.slice(p)
        def map[B](f: A => B): Parser[B] = self.map(p)(f)
        def ** [B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
        def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
        def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    }

    // Error handling
    // replace the error message with msg
    def label[A](msg: String)(p: Parser[A]): Parser[A]

    // append msg to error message stack
    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]

    // Exercise 9.16
    def reportError(e: ParseError): String = e.stack.groupBy(s => s._1).map(
        x => "Error at line %d, col %d\n".format(x._1.line, x._1.col) + x._2.foldLeft("")((b, c) => "\t" + b + c._2 + "\n")
    ).foldLeft("")((b, s) => b + s)

    // def errorLocation(e: ParseError): Location

    // def errorMessage(e: ParseError): String
}
