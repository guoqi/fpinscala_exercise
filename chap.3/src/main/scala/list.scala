package main

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons[A](as.head, List(as.tail: _*))
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // Result of Exercise 3.1 is 3

    // Exercise 3.2
    def tail[A](l: List[A]): List[A] = l match {
        case Nil => throw new IndexOutOfBoundsException
        case Cons(x, xs) => xs
    }

    def head[A](l: List[A]): A = l match {
        case Nil => throw new IndexOutOfBoundsException
        case Cons(x, _) => x
    }

    // Exercise 3.3
    def setHead[A](l: List[A], h: A): List[A] = l match {
        case Nil => throw new IndexOutOfBoundsException
        case Cons(_, xs) => Cons(h, xs)
    }

    // Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] = {
        if (n <= 0) l
        else l match {
            case Nil => throw new IndexOutOfBoundsException
            case Cons(_, xs) => drop(xs, n - 1)
        }
    }

    // Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

    // Exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    // Exercise 3.7
    // Answer: it is possible to make a short-circuiting when it encounters a 0.0 by lazy execution

    // Exercise 3.8
    // Answer: foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) will result in List(1, 2, 3)

    // Exercise 3.9
    def length[A](l: List[A]): Int = l match {
        case Nil => 0
        case Cons(_, xs) => 1 + length(xs)
    }

    // Exercise 3.10
    @annotation.tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // Exercise 3.11
    def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
    def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    // Exercise 3.12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((t: List[A], h: A) => Cons(h, t))

    // Exercise 3.13
    // implement foldLeft in terms of foldRight
    def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

    // implement foldRight in terms of foldLeft
    def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = ???

    // Exercise 3.14
    // implement append in terms of foldLeft
    def append[A](a1: List[A], a2: List[A]): List[A] = {
        def appendTail(l: List[A], t: A): List[A] = l match {
            case Nil => Cons(t, Nil)
            case Cons(x, xs) => Cons(x, appendTail(xs, t))
        }
        foldLeft(a2, a1)(appendTail)
    }

    // implement append in terms of foldRight
    def append2[A](a1: List[A], a2: List[A]): List[A] = {
        foldRight(a1, a2)(Cons(_, _))
    }

    // Exercises 3.15
    def Concat[A](ls: List[List[A]]): List[A] = ls match {
        case Nil => Nil
        case Cons(x, xs) => append(x, Concat(xs))
    }

    // Exercise 3.16
    def add_by_1(l: List[Int]): List[Int] = l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x + 1, add_by_1(xs))
    }

    // Exercise 3.17
    def stringify(l: List[Double]): List[String] = l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x.toString, stringify(xs))
    }

    // Exercise 3.18
    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    // Exercise 3.19
    def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) => if (p(x))  Cons(x, filter(xs)(p)) else filter(xs)(p)
    }

    def removeOddInt(l: List[Int]): List[Int] = filter(l)(x => x % 2 == 0)

    // Exercise 3.20
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
        case Nil => Nil
        case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }

    // Exercise 3.21
    def filter2[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(x => if(p(x)) List(x) else Nil)

    // Exercise 3.22
    def zipAdd(l: List[Int], g: List[Int]): List[Int] = l match {
        case Nil => g
        case Cons(x, xs) => g match {
            case Nil => l
            case Cons(y, ys) => append(List(x + y), zipAdd(xs, ys))
        }
    }

    // Exercise 3.23
    def zipWith[A](l: List[A], g: List[A])(f: (A, A) => A): List[A] = l match {
        case Nil => g
        case Cons(x, xs) => g match {
            case Nil => l
            case Cons(y, ys) => append(List(f(x, y)), zipWith(xs, ys)(f))
        }
    }

    // Exercise 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        def comp(a1: List[A], a2: List[A]): Boolean = a1 match {
            case Nil => a2 == Nil
            case Cons(x, xs) => a2 match {
                case Nil => true
                case Cons(y, ys) => x == y && comp(xs, ys)
            }
        }

        sup match {
            case Nil => sub == Nil
            case Cons(x, xs) => sub match {
                case Nil => true
                case Cons(y, ys) => if (x == y && comp(xs, ys)) true else hasSubsequence(xs, sub)
            }
        }
    }
}

