import language.higherKinds
import language.reflectiveCalls
import language.implicitConversions
import language.existentials
import random.State

trait Applicative[F[_]] {
    // primitive combinators
    def unit[A](a: => A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    // derived combinators
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight[F[List[B]]](unit(Nil))((a, b) => map2(f(a), b)(_ :: _))

    def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

    // Exercise 12.1
    def sequence[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
        if (n == 0) unit(List[A]())
        else map2(fa, replicateM(n - 1, fa))((a, as) => a::as)
    }

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    // Exercise 12.2
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((fa, a) => fa(a))

    def map_2[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

    def map2_2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

    // Exercise 12.3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    // Exercise 12.12
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldRight(unit(Map[K, V]()))((m, fb) => map2(m._2, fb)((v, kv) => Map(m._1 -> v) ++ kv))
}

trait Monad[F[_]] extends Applicative[F] {
    // primitive combinators
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def unit[A](a: => A): F[A]

    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

    override def map2[A, B, C] (fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => flatMap(fb)(b => unit(f(a, b))))
}

object Monad {
    def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
        override def flatMap[A, B] (fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
        override def unit[A] (a: => A): State[S, A] = State.unit(a)
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monoid[A] {
    def zero: A
    def op(a1: A, a2: A): A
}

trait Foldable[F[_]] {
    def foldRight[A, B](fa: F[A])(z: B)(f: (A, B) => B): B = {
        val mb: Monoid[B => B] = new Monoid[B => B] {
            override def zero: B => B = n => n
            override def op (a1: B => B, a2: B => B): B => B = n => a1(a2(n))
        }
        foldMap(fa)(a => f(a, _: B))(mb)(z)
    }

    def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = {
        val mb: Monoid[B => B] = new Monoid[B => B] {
            override def zero: B => B = n => n
            override def op (a1: B => B, a2: B => B): B => B = n => a2(a1(n))
        }
        foldMap(fa)(a => f(_: B, a))(mb)(z)
    }

    def foldMap[A, B](fa: F[A])(f: A => B)(m: Monoid[B]): B
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit g: Applicative[G]): G[F[B]]
    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

    private type ConstM[A] = A
    private implicit val constMApplicative: Applicative[ConstM] = new Applicative[ConstM] {
        override def unit[A] (a: => A): ConstM[A] = a
        override def map2[A, B, C] (fa: ConstM[A], fb: ConstM[B])(f: (A, B) => C): ConstM[C] = f(fa, fb)
    }

    private type Const[M, B] = M
    private implicit def monoidApplicative[M](m: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] = new Applicative[({type f[x] = Const[M, x]})#f] {
        override def unit[A] (a: => A): Const[M, A] = m.zero
        override def map2[A, B, C] (fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = m.op(fa, fb)
    }

    //  Exercise 12.14
    override def map[A, B] (fa: F[A])(f: A => B): F[B] = traverse[ConstM, A, B](fa)(f)  // use constMApplicative implicitly

    override def foldMap[A, B] (fa: F[A])(f: A => B)(m: Monoid[B]): B = traverse[({type f[x] = Const[B, x]})#f, A, Nothing](fa)(f)  // use monoidApplicative implicitly

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)((a: A) => for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
    } yield b).run(s)

    // Exercise 12.16
    def reverse[A](fa: F[A]): F[A] = ???

    // Exercise 12.17
    def foldLeft[A, B](fa: F[A])(z: B)(f: (A, B) => B): B = mapAccum[B, A, B](fa, z)((a, b) => (b, f(a, b)))._2

    // Exercise 12.18
    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???
        // traverse(fa)(a => )(Exercise.product(G, H))
}

trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Exercise {
    val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
        def unit[A](a: => A) : Stream[A] = Stream.continually(a)

        def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = fa zip fb map f.tupled
    }

    // Exercise 12.4
    // streamApplicative.sequence means to concatenate all Stream value in list and wraps it with Stream

    // Exercise 12.5
    def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
        override def flatMap[A, B] (fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
            case Left(e) => Left(e)
            case Right(v) => f(v)
        }

        override def unit[A] (a: => A): Either[E, A] = Right(a)
    }

    // Exercise 12.6
    def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
        override def unit[A] (a: => A): Validation[E, A] = Success(a)

        override def map2[A, B, C] (fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
            case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, (t1 :+ h2) ++: t2)
            case (Failure(h, t), Success(_)) => Failure(h, t)
            case (Success(_), Failure(h, t)) => Failure(h, t)
            case (Success(v1), Success(v2)) => Success(f(v1, v2))
        }
    }

    // Exercise 12.7 TODO

    // Exercise 12.8
    def product[F[_], G[_]] (F: Applicative[F], G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A] (a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))

        override def map2[A, B, C] (fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
            (F.map(F.product(fa._1, fb._1))(f.tupled), G.map(G.product(fa._2, fb._2))(f.tupled))
    }

    // Exercise 12.9
    def compose[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A] (a: => A): F[G[A]] = F.unit(G.unit(a))

        override def map2[A, B, C] (fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f(_, _)))
    }

    // Exercise 12.10 TODO

    // Exercise 12.11
    def compose[F[_], G[_]](F: Monad[F], G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
        override def unit[A] (a: => A): F[G[A]] = F.unit(G.unit(a))

        // It's not possible because parameter f returns a type F[G[B]] and a nested type cannot be joined or expanded
        override def flatMap[A, B] (fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = ???
    }

    // Exercise 12.13
    val listTraverse: Traverse[List] = new Traverse[List] {
        override def traverse[G[_] : Applicative, A, B] (fa: List[A])(f: A => G[B])(implicit g: Applicative[G]): G[List[B]] = g.traverse(fa)(f)
    }

    val optionTraverse: Traverse[Option] = new Traverse[Option] {
        override def traverse[G[_] : Applicative, A, B] (fa: Option[A])(f: A => G[B])(implicit g: Applicative[G]): G[Option[B]] = fa match {
            case None => g.unit(None)
            case Some(x) => g.map(f(x))(Some(_))
        }
    }

    val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
        override def traverse[G[_] : Applicative, A, B] (fa: Tree[A])(f: A => G[B])(implicit g: Applicative[G]): G[Tree[B]] = fa match {
            case Leaf(v) => g.map(f(v))(Leaf(_))
            case Branch(l, r) => g.map2(traverse(l)(f), traverse(r)(f))((left, right) => Branch(left, right))
        }
    }

    // Exercise 12.15 TODO

    // Exercise 12.19
    def compose[F[_], G[_]](F: Traverse[F], G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[H[_] : Applicative, A, B] (fa: F[G[A]])(f: A => H[B])(implicit h: Applicative[H]): H[F[G[B]]] =
            F.traverse(fa)(ga => G.traverse(ga)(f))
    }

    // Exercise 12.20
    // G is a monad and also traversable
    def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
        override def flatMap[A, B] (fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = F.flatMap(fa)(ga => F.map(T.traverse(ga)(a => f(a)))(gga => G.join(gga)))

        override def unit[A] (a: => A): F[G[A]] = F.unit(G.unit(a))
    }
}