package main

object App{
    // Exercise 2.1
    def fib(n: Int): Int = {
        @annotation.tailrec
        def loop(i: Int, pre: Int, cur: Int): Int = {
            if (n == i) pre + cur
            else loop(i + 1, cur, pre + cur)
        }
        if (n == 0 || n == 1) n
        else loop(2, 0, 1)
    }

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A,A)=>Boolean): Boolean = {
        if (as.length == 0 || as.length == 1) true
        else ordered(as(0), as(1)) && isSorted(as.tail, ordered)
    }

    // Exercise 2.3
    def curry[A, B, C](f: (A, B) => C): A => B => C = {
        def fn(a: A)(b: B): C = {
            f(a, b)
        }
        fn
    }

    // Exercise 2.4
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
        def fn(a: A, b: B): C = f(a)(b)
        fn
    }

    // Exercise 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        def fn(a: A): C = f(g(a))
        fn
    }

    def main(args: Array[String]): Unit = {
        // do nothing
    }
}