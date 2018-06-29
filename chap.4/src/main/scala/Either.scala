// Exercise 4.6
sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](op: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B] (f: Nothing => B): Either[E, B] = Left(value)

    override def flatMap[EE >: E, B] (f: Nothing => Either[EE, B]): Either[EE, B] = Left(value)

    override def orElse[EE >: E, B >: Nothing] (op: => Either[EE, B]): Either[EE, B] = op

    override def map2[EE >: E, B, C] (b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = Left(value)
}

case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B] (f: A => B): Either[Nothing, B] = Right(f(value))

    override def flatMap[EE >: Nothing, B] (f: A => Either[EE, B]): Either[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A] (op: => Either[EE, B]): Either[EE, B] = this

    override def map2[EE >: Nothing, B, C] (b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
        case Right(v) => Right(f(value, v))
        case Left(e) => Left(e)
    }
}

object Either {
    // Exercise 4.7
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(Nil)
        case x::xs => f(x) match {
            case Left(e) => Left(e)
            case Right(v) => xs.foldLeft[Either[E, List[B]]](Right(List(v)))((b, c) => b.map2(f(c))((vs, v) => vs:+v))
        }
    }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
}

/*
// Exercise 4.8
// Need some type and type of data structure knowledge
sealed trait MultiEither[+Seq[E], +A] {
    def map[B] (f: A => B): MultiEither[Seq[_], B]
    def flatMap[EE >: E, B] (f: A => MultiEither[Seq[EE], B]): MultiEither[Seq[EE], B]
    def orElse[B >: A](op: => MultiEither[Seq[_], B]): MultiEither[Seq[_], B]
    def map2[B, C](b: MultiEither[Seq[_], B])(f: (A, B) => C): MultiEither[Seq[_], C]
}

case class MultiLeft(value: Seq[_]) extends MultiEither[Seq[_], Nothing] {
    override def map[B] (f: Nothing => B): MultiEither[Seq[_][_], B] = ???

    override def flatMap[B] (f: Nothing => MultiEither[Seq[_][_], B]): MultiEither[Seq[_][_], B] = ???

    override def orElse[B >: Nothing] (op: => MultiEither[Seq[_][_], B]): MultiEither[Seq[_][_], B] = ???

    override def map2[B, C] (b: MultiEither[Seq[_][_], B])(f: (Nothing, B) => C): MultiEither[Seq[_][_], C] = ???
}

case class MultiRight[A](value: A) extends MultiEither[Nothing, A] {
    override def map[B] (f: A => B): MultiEither[Nothing[_], B] = MultiRight(f(value))

    override def flatMap[B] (f: A => MultiEither[Nothing[_], B]): MultiEither[Nothing[_], B] = f(value)

    override def orElse[B >: A] (op: => MultiEither[Nothing[_], B]): MultiEither[Nothing[_], B] = op

    override def map2[B, C] (b: MultiEither[Nothing[_], B])(f: (A, B) => C): MultiEither[Nothing[_], C] = b match {
        case MultiRight(v) => MultiRight(f(value, v))
        case MultiLeft(es) => MultiLeft(es)
    }
}
*/
