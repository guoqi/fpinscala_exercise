sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def apply(in: Input): Machine = (in, locked, candies) match {
        case (_, _, 0) => this
        case (Coin, true, _) => Machine(locked = false, candies, coins + 1)
        case (Turn, false, _) => Machine(locked = true, candies - 1, coins)
        case _ => this
    }
}

object Machine {
    // Exercise 6.11
    def simulateMachine (inputs: List[Input]): State[Machine, (Int, Int)] = State(m => inputs match {
        case Nil => ((m.coins, m.candies), m)
        case x :: xs => {
            val m2 = m(x)
            val m3 = simulateMachine(xs)(m2)
            m3
        }
    })
}
