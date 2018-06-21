package main

sealed trait Tree[A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // Exercise 3.25
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => size(l) + size(r)
    }

    // Exercise 3.26
    def maximum(tree: Tree[Int]): Int = tree match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    // Exercise 3.27
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => (depth(l) max depth(r)) + 1
    }

    // Exercise 3.28
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // Exercise 3.29
    def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B, g: (B, B) => B): B = tree match {
        case Leaf(v) => f(v, z)
        case Branch(l, r) => g(fold(l, z)(f, g), fold(r, z)(f, g))
    }

    def size2[A](tree: Tree[A]): Int = fold(tree, 1)((_, b) => b, _ + _)

    def maximum2(tree: Tree[Int]): Int = fold(tree, 0)((a, _) => a, _ max _)

    def depth2[A](tree: Tree[A]): Int = fold(tree, 1)((_, _) => 1, _.max(_) + 1)

    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree, null)((v, _) => Leaf(f(v)), Branch(_, _))
}
