import check.Gen
import check.Prop

import scala.annotation.tailrec

/*
 * A Monoid is a set of specified type which has a 'zero' value and associative operation op
 * op(zero, x) == op(x, zero) == x && op(x, op(y, z)) == op(op(x, y), z)
 */

trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Foldable[F[_]]{
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    // Exercise 10.15
    def toList[A](fa: F[A]): List[A] = foldLeft[A, List[A]](fa)(Nil)((l, x) => x::l)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Exercise {
    // Exercise 10.1
    val intAddition: Monoid[Int] = new Monoid[Int] {
        override def op (a1: Int, a2: Int): Int = a1 + a2

        override def zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
        override def op (a1: Int, a2: Int): Int = a1 * a2

        override def zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
        override def op (a1: Boolean, a2: Boolean): Boolean = a1 || a2

        override def zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
        override def op (a1: Boolean, a2: Boolean): Boolean = a1 && a2

        override def zero: Boolean = true
    }

    // Exercise 10.2
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
        override def op (a1: Option[A], a2: Option[A]): Option[A] = a1 match {
            case Some(x) => a1
            case None => a2
        }

        override def zero: Option[A] = None
    }

    // Exercise 10.3
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
        // op(op(x, y), z)  ==== n => (n => x(y(n)))(z(n)) === n => x(y(z(n)))
        // op(x, op(y, z))  ==== n => x(y(z(n)))
        override def op (a1: A => A, a2: A => A): A => A = n => a1(a2(n))

        override def zero: A => A = n => n
    }

    // Exercise 10.4
    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
        Prop.forAll(gen)(a => m.op(a, m.zero) == m.op(m.zero, a)) &&
            Prop.forAll(gen ** gen ** gen)(elem => m.op(m.op(elem._1._1, elem._1._2), elem._2) == m.op(elem._1._1, m.op(elem._1._2, elem._2)))
    }

    // Exercise 10.5
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)

    // Exercise 10.6
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
        val m: Monoid[B => B] = new Monoid[B => B] {
            override def op (a1: B => B, a2: B => B): B => B = n => a2(a1(n))

            override def zero: B => B = n => n
        }

        foldMap(as, m)(a => f(_: B, a))(z)
    }

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(a => f(a, _: B))(z)

    // Exercise 10.7
    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
        if (v.length < 2) v.map(f).foldLeft(m.zero)(m.op)
        else {
            val (left, right) = v.splitAt(v.length / 2)
            m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
        }
    }

    // Exercise 10.8
    // TODO


    // Exercise 10.9
    def isOrderd(v: IndexedSeq[Int]): Boolean = ???

    // Exercise 10.10
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
        override def op (a1: WC, a2: WC): WC = (a1, a2) match {
            case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
            case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
            case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
            case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (r1 + l2).split("[^a-zA-Z]").length, r2)
        }

        override def zero: WC = Stub("")
    }

    // Exercise 10.11
    def wordCount(s: String): Int = foldMapV(s, wcMonoid)(c => Stub(c.toString)) match {
        case Stub(s1) => if (s1.isEmpty) 0 else 1
        case Part(l, w, r) => {
            val lc = if (l.isEmpty) 0 else 1
            val rc = if (r.isEmpty) 0 else 1
            lc + w + rc
        }
    }

    // Exercise 10.12
    object flist extends Foldable[List] {
        override def foldLeft[A, B] (as: List[A])(z: B)(f: (B, A) => B): B = {
            @tailrec
            def helper(as: List[A])(r: B): B = as match {
                case Nil => r
                case x::xs => helper(xs)(f(r, x))
            }
            helper(as)(z)
        }

        override def foldRight[A, B] (as: List[A])(z: B)(f: (A, B) => B): B = as match {
            case Nil => z
            case x::xs => f(x, foldRight(xs)(z)(f))
        }

        override def foldMap[A, B] (as: List[A])(f: A => B)(mb: Monoid[B]): B = {
            // a few like foldLeft.
            // Notice: List has an efficient head and tail, and also can be pattern matching
            @tailrec
            def helper(as: List[A])(r: B): B = as match {
                case Nil => r
                case x::xs => helper(xs)(mb.op(f(x), r))
            }
            helper(as)(mb.zero)
        }
    }

    object findexedseq extends Foldable[IndexedSeq] {
        override def foldLeft[A, B] (as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

        override def foldRight[A, B] (as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

        override def foldMap[A, B] (as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
            // Notice: IndexedSeq has an efficient apply(locate by index) and length, but it doesn't have an efficient head and tail
            //         That's reason of differences of the two implementation
            @tailrec
            def helper(i: Int)(b: B): B = {
                if (i >= as.length) b
                else {
                    helper(i + 1)(mb.op(f(as(i)), b))
                }
            }
            helper(0)(mb.zero)
        }
    }

    object fstream extends Foldable[Stream] {
        override def foldLeft[A, B] (as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

        override def foldRight[A, B] (as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

        override def foldMap[A, B] (as: Stream[A])(f: A => B)(mb: Monoid[B]): B = {
            def helper(s: Stream[A])(r: B): B = s match {
                case Stream.Empty => r
                case x#::xs => helper(xs)(mb.op(f(x), r))
            }
            helper(as)(mb.zero)
        }
    }

    // Exercise 10.13
    object ftree extends Foldable[Tree] {
        override def foldLeft[A, B] (as: Tree[A])(z: B)(f: (B, A) => B): B = {
            def helper(t: Tree[A])(result: B): B = t match {
                case Leaf(v) => f(result, v)
                case Branch(l, r) => helper(r)(helper(l)(result))
            }
            helper(as)(z)
        }

        override def foldRight[A, B] (as: Tree[A])(z: B)(f: (A, B) => B): B = {
            def helper(t: Tree[A])(result: B): B = t match {
                case Leaf(v) => f(v, result)
                case Branch(l, r) => helper(l)(helper(r)(result))
            }
            helper(as)(z)
        }

        override def foldMap[A, B] (as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
            def helper(t: Tree[A])(result: B): B = t match {
                case Leaf(v) => mb.op(f(v), result)
                case Branch(l, r) => helper(r)(helper(l)(result))
            }
            helper(as)(mb.zero)
        }
    }

    // Exercise 10.14
    object foption extends Foldable[Option] {
        override def foldLeft[A, B] (as: Option[A])(z: B)(f: (B, A) => B): B = as match {
            case None => z
            case Some(x) => f(z, x)
        }

        // Option type is not a seq-like type, so foldRight is just as the same as foldLeft
        override def foldRight[A, B] (as: Option[A])(z: B)(f: (A, B) => B): B = foldLeft(as)(z)((b: B, a: A) => f(a, b))

        override def foldMap[A, B] (as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
            case None => mb.zero
            case Some(x) => mb.op(f(x), mb.zero)
        }
    }

    // Exercise 10.16
    def pruductMonoid[A, B](am: Monoid[A], bm: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        override def op (a1: (A, B), a2: (A, B)): (A, B) = (am.op(a1._1, a2._1), bm.op(a1._2, a2._2))

        override def zero: (A, B) = (am.zero, bm.zero)
    }

    // Exercise 10.17
    def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
        override def op (a1: A => B, a2: A => B): A => B = a => mb.op(a1(a), a2(a))

        override def zero: A => B = _ => mb.zero
    }

    def mapMerMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
        override def op (a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keySet ++ a2.keySet).foldLeft(zero) {
            (acc, k) => acc.updated(k, mv.op(a1.getOrElse(k, mv.zero), a2.getOrElse(k, mv.zero)))
        }

        override def zero: Map[K, V] = Map[K, V]()
    }

    // Exercise 10.18
    // convert and compose
    // such a compact and elegant coding style !
    def bag[A](as: IndexedSeq[A]): Map[A, Int] = findexedseq.foldMap(as)(a => Map(a -> 1))(mapMerMonoid[A, Int](intAddition))
}
