import random._

trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def unit[A](a: => A): F[A]

    override def map[A, B] (fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => flatMap(fb)(b => unit(f(a, b))))

    // Exercise 11.3
    def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
        case Nil => unit(Nil)
        case x :: xs => map2(x, sequence(xs))((a, as) => a :: as)
    }

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la match {
        case Nil => unit(Nil)
        case x :: xs => map2(f(x), traverse(xs)(f))((a, as) => a :: as)
    }

    // Exercise 11.4
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

    // Exercise 11.5
    /*
     * replicateM behaves in the List monad, just as below:
     * from List(List(1,2,3), List(1,2,3)) ===> List(List(1,1), List(1,2), List(1,3), List(2,1), List(2,2), List(2,3), List(3,1), List(3,2), List(3,3))
     *
     * and behaves in the Option monad, as below:
     * List(Some(1), Some(1), Some(1), Some(1)) ===> Some(List(1,1,1,1))
     * List(None, None) ===> None
     *
     * and replicateM is like * in BNF
     */

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    // Exercise 11.6
    // Some a little like traverse but there is an additional judgement based on evaluation of f
    def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = la match {
        case Nil => unit(Nil)
        case x :: xs => flatMap(f(x))(b => if (b) map2(unit(x), filterM(xs)(f))((a, as) => a :: as) else filterM(xs)(f))
    }

    // Exercise 11.7
    /* compose(compose(f, g), h) ===> compose(a => flatMap(f(a))(b => g(b))), h)
    *                            ===> a => flatMap(flatMap(f(a))(b => g(b)))(c => h(c))
    *                            ---> (flatMap(f(a))(b => g(b))).flatMap(c => h(c))   -------- omits parameter a
    *                            ===> (f(a).flatMap(b => g(b)).flatMap(c => h(c))
    *                            ===> f(a).flatMap(b => g(b)).flatMap(c => h(c))
    *                            ===> f.flatMap(g).flatMap(h)                         -------- let f == f(a), g == b => g(b), h == c => h(c)
    *  compose(f, compose(g, h)) ===> compose(f, b => flatMap(g(b))(c => h(c)))
    *                            ===> a => flatMap(f(a))(b => flatMap(g(b))(c => h(c)))
    *                            ---> f(a).flatMap(b => flatMap(g(b))(c => h(c)))
    *                            ===> f(a).flatMap(b => g(b).flatMap(c => h(c)))
    *                            ===> f.flatMap(b => g(b).flatMap(h))                 -------- let f == f(a), g == b => g(b), h == c => h(c)
    *
    *  And here, compose function also obeys associative law as the same as flatMap
    */
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(b => g(b))

    // Exercise 11.8
    def flatMap2[A, B](fa: F[A])(f: A => F[B]): F[B] = compose[Null, A, B](Null => fa, f)(null)

    // Exercise 11.9
    // see the comment of Exercise 11.7

    // Exercise 11.10
    /*
     * flatMap(x)(unit) ===> flatMap(x)(a => unit(a)) ===> x
     * flatMap(unit(y))(f) ===> flatMap(unit(y))(a => f(a)) ===> f(y)
     */

    // Exercise 11.11
    /*
     * As for listMonad:
     *      flatMap(x)(unit) ===> flatMap(x)(a => List(a)) ===> x
     *      flatMap(unit(y))(f) ===> flatMap(List(y))(f) ===> flatMap(List(y))(a => f(a)) ===> f(y)
     */

    // Exercise 11.12
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

    // Exercise 11.13
    def flatMap3[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    def compose2[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

    // Exercise 11.14
    /*
     * Identity laws:
     *      join(map(ma)(unit))      == ma
     *      join(map(unit(x))(f))    == f(x)
     * Associative laws:
     *      join(map(join(map(x)(f)))(g)) == join(map(x)(a => join(map(f(a))(h))))
     */

    // Exercise 11.15 TODO

    // Exercise 11.16 TODO
}

// Exercise 11.17
case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)

    def map[B](f: A => B): Id[B] = Id(f(value))
}

object Monad {
    // Exercise 11.1
    val optionMonad: Monad[Option] = new Monad[Option] {
        override def flatMap[A, B] (fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
            case Some(x) => f(x)
            case None => None
        }

        override def unit[A] (a: => A): Option[A] = Some(a)
    }

    val streamMonad: Monad[Stream] = new Monad[Stream] {
        override def flatMap[A, B] (fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa match {
            case x #:: xs => f(x) ++ flatMap(xs)(f)
            case Stream.Empty => Stream.Empty
        }

        override def unit[A] (a: => A): Stream[A] = a #:: Stream.Empty
    }

    val listMonad: Monad[List] = new Monad[List] {
        override def flatMap[A, B] (fa: List[A])(f: A => List[B]): List[B] = fa match {
            case Nil => Nil
            case x :: xs => f(x) ++ flatMap(xs)(f)
        }

        override def unit[A] (a: => A): List[A] = List(a)
    }

    // Exercise 11.2
    // partial type application
    def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f]  = new Monad[({type f[x] = State[S, x]})#f] {
        override def flatMap[A, B] (fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f

        override def unit[A] (a: => A): State[S, A] = State.unit(a)
    }

    // Exercise 11.17
    val idMonad: Monad[Id] = new Monad[Id] {
        override def flatMap[A, B] (fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f

        override def unit[A] (a: => A): Id[A] = Id(a)
    }

    // Exercise 11.18 TODO

    // Exercise 11.19 TODO
}

// Exercise 11.20
// It is some a little like State monad, TODO
case class Reader[R, A](run: R => A)

object Reader {
    def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {
        override def flatMap[A, B] (fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???

        override def unit[A] (a: => A): Reader[R, A] = ???
    }
}