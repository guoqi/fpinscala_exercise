// Exercise 4.1
sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = None
    def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    def getOrElse[B >: Nothing](default: => B): B = default
    def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
    def filter(f: Nothing => Boolean): Option[Nothing] = None
}

case class Some[+A](value: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(value))
    def flatMap[B](f: A => Option[B]): Option[B] = f(value)
    def getOrElse[B >: A](default: => B): B = value
    def orElse[B >: A](ob: => Option[B]): Option[B] = this
    def filter(f: A => Boolean): Option[A] = if (f(value)) this else None
}

// companion object
object Option {
    def mean(xs: Seq[Double]): Option[Double] = {
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
    }

    // Exercise 4.2
    def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => {
        val ys = xs.map(x => math.pow(x - m, 2))
        Some(ys.sum / ys.length)
    })

    // Exercise 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

    // Exercise 4.4
    // List[Option[A]] => Option[List[A]]
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
        a match {
            case Nil => Some(Nil)
            case x::xs => x match {
                case None => None
                case Some(v) => xs.foldLeft[Option[List[A]]](Some(List(v)))((b, c) => map2(b, c)((vs, v) => vs :+ v))
            }
        }
    }

    // Exercise 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
        // Option[List[A]] => Option[List[B]]
        a match {
            case Nil => Some(Nil)
            case x :: xs => f(x) match {
                case None => None
                case Some(v) => xs.foldLeft[Option[List[B]]](Some(List(v)))((b, c) => map2(b, f(c))((vs, v) => vs :+ v))
            }
        }
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}
