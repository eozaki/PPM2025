package Aula05

sealed trait MyTree[+A]
case object Empty extends MyTree[Nothing]
case class Node[A](value: A, left: MyTree[A], right: MyTree[A]) extends MyTree[A]
case class Ex1[A](myField: MyTree[A]) {
  def maximum() = Ex1.maximum(this.myField.asInstanceOf[MyTree[Int]])
  def depth() = Ex1.depth(this.myField)
  def map[B](f: A => B):MyTree[B] = Ex1.map(this.myField)(f)
}

object Ex1{
  def maximum(t: MyTree[Int]): Option[Int] = t match {
    case Empty => None
    case Node(value, left, right) => {
      val l = maximum(left)
      val r = maximum(right)

      val m = max(value, l)
      Some(max(m, r))
    }
  }

  def max(a: Int, b: Option[Int]): Int = {
    if(b.isEmpty || a > b.get) a
    else b.get
  }

  def depth[A](t: MyTree[A]): Int = t match {
    case Empty => 0
    case Node(_, left, right) => 1 + max(depth(left), Some(depth(right)))
  }

  def map[A,B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case Empty => Empty
    case Node(v, l, r) => Node(f(v), map(l)(f), map(r)(f))
  }

  def main(args: Array[String]): Unit = {
    val tree1 = Node(8, Node(42, Empty, Empty), Empty)
    val t = Ex1(tree1)
    println(s"Maximum element of the tree: ${ t.maximum() }")
    println(s"Depth of the tree: ${ t.depth() }")
    println(s"Map: ${ t.map((x: Int) => x * 2 ) }")
  }
}