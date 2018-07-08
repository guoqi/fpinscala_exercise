sealed trait Stream[+A] {
    // Exercise 5.1
    def toList: List[A]

    // Exercise 5.2
    def take(n: Int): Stream[A]
    def drop(n: Int): Stream[A]

    // Exercise 5.3
    def takeWhile(p: A => Boolean): Stream[A]

    // Exercise 5.4
    def forAll(p: A => Boolean): Boolean

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    // Exercise 5.5
    def takeWhile2(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, as) => if (! p(a)) Empty else Stream.cons(a, as))

    // Exercise 5.6
    def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

    // Exercise 5.7
    def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((a, bs) => Stream.cons(f(a), bs))
    def filter(f: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, as) => if(f(a)) Stream.cons(a, as) else as)
    // Notice: covariant type cannot be method parameter, which will cause a 'covariant type T occurs in contravariant position'
    def append[B >: A](a: => Stream[B]): Stream[B] = foldRight[Stream[B]](a)((a1, a2) => Stream.cons(a1, a2))
    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, bs) => f(a).append(bs))

    def exists(p: A => Boolean): Boolean
}

case object Empty extends Stream[Nothing] {
    def toList: List[Nothing] = Nil
    def take(n: Int): Stream[Nothing] = Empty
    def drop(n: Int): Stream[Nothing] = Empty
    def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
    def forAll(p: Nothing => Boolean): Boolean = true
    def exists(p: Nothing => Boolean): Boolean = false
}
// Notice: parameter in case class is not allowed call by name, so here we just wrap it with a empty parameter function
case class Cons[+A](hd: () => A, tail: () => Stream[A]) extends Stream[A] {
    def toList: List[A] = hd() :: tail().toList
    def take(n: Int): Stream[A] = if (n > 0) Stream.cons(hd(), tail().take(n - 1)) else Empty
    def drop(n: Int): Stream[A] = if (n == 0) this else tail().drop(n - 1)
    def takeWhile (p: A => Boolean): Stream[A] = if (p(hd())) Stream.cons(hd(), tail().takeWhile(p)) else Empty
    def forAll(p: A => Boolean): Boolean = p(hd()) && tail().forAll(p)
    def exists(p: A => Boolean): Boolean = p(hd()) || tail().exists(p)
}

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A]  = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // Exercise 5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // Exercise 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    // Exercise 5.10
    def fib(n: Int): Int = {
        if (n == 1 || n == 2) 1
        else {
            lazy val a = fib(n - 1)
            lazy val b = fib(n - 2)
            a + b
        }
    }

    def fibs: Stream[Int] = {
        def go(n: Int): Stream[Int] = cons(fib(n), go(n+1))
        go(1)
    }

    // Exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case None => empty
        case Some(x) => cons(x._1, unfold(x._2)(f))
    }

    // Exercise 5.12
    def fibs2: Stream[Int] = unfold(1)(n => Some((fib(n), n+1)))

    def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n, n+1)))

    def constant2[A](a: A): Stream[A] = unfold[A, A](a)(a => Some((a, a)))

    def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

    // Exercise 5.13
    def map[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold[B, Stream[A]](s)({
        case Empty => None
        case Cons(hd, tail) => Some((f(hd()), tail()))
    })

    def take[A](s: Stream[A], n: Int): Stream[A] = unfold[A, Stream[A]](s)({
        case Empty => None
        case Cons(hd, tail) => if (n == 0) None else Some((hd(), take(tail(), n - 1)))
    })

    def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = unfold[A, Stream[A]](s)({
        case Empty => None
        case Cons(hd, tail) => if(! p(hd())) None else Some((hd(), takeWhile(tail())(p)))
    })

    def zipWith[A](s1: Stream[A], s2: Stream[A])(f: (A, A) => A): Stream[A] = unfold[A, (Stream[A], Stream[A])]((s1, s2))({
        case (Empty, Empty) => None
        case (Cons(hd, tail), Empty) => Some((hd(), (tail(), Empty)))
        case (Empty, Cons(hd, tail)) => Some((hd(), (Empty, tail())))
        case (Cons(hd1, tail1), Cons(hd2, tail2)) => Some((f(hd1(), hd2()), (tail1(), tail2())))
    })

    def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((s1, s2))({
        case (Empty, Empty) => None
        case (Cons(hd, tail), Empty) => Some((Some(hd()), None), (tail(), Empty))
        case (Empty, Cons(hd, tail)) => Some((None, Some(hd())), (Empty, tail()))
        case (Cons(hd1, tail1), Cons(hd2, tail2)) => Some((Some(hd1()), Some(hd2())), (tail1(), tail2()))
    })

    // Exercise 5.14
    def startsWith[A](s: Stream[A], s1: Stream[A]): Boolean = zipAll(s, s1).forAll(a => a._1.isDefined && (a._2.isEmpty || a._2 == a._1))

    // Exercise 5.15
    def tails[A](s: Stream[A]): Stream[Stream[A]] = unfold[Stream[A], Stream[A]](s)({
        case Empty => None
        case Cons(hd, tail) => Some((Cons(hd, tail), tail()))
    })

    def hasSubSequence[A](s: Stream[A], s1: Stream[A]): Boolean = tails(s).exists(startsWith(_, s1))

    // Exercise 5.16
    // Not time linear !!!!
    // TODO
    def scanRight[A](s: Stream[A], z: A)(f: (A, A) => A): Stream[A] = unfold[A, Stream[A]](s)({
        case Empty => None
        case Cons(hd, tail) => Some((Cons(hd, tail).foldRight(z)((x, y) => f(x, y)), tail()))
    })
}
