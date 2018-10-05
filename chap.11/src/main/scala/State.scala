package random

// Exercise 6.10
case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](s => {
        val (a, s2) = run(s)
        val (b, s3): (B, S) = f(a)(s2)
        (b, s3)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.flatMap(b => State.unit(f(a, b))))

    def apply(s: S): (A, S) = run(s)
}

object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => fs match {
        case Nil => (Nil, s)
        case x :: xs => {
            val (a, s2) = x(s)
            val (as, s3) = sequence(xs)(s2)
            (a :: as, s3)
        }
    })
}
