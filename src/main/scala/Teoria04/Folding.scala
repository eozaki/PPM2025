package Teoria04

object Folding {
  def concat[T](l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
    case (Nil, y) => y
    case (x :: xs, y) => x :: concat(xs, y)
  }

  def concatFR[T](l1: List[T], l2: List[T]): List[T] = {
    (l1 foldRight l2) ((x, r) => x :: r)
    // r is the recursive invocation over the remainder of the l1 list, i.e., every element except the first
  }

  def concatFL[T](l1: List[T], l2: List[T]): List[T] = {
    (l2 foldLeft l1) ((r, x) => r :+ x)
    // r is the recursive invocation over the remainder of the l1 list, i.e., every element except the last
  }

  def mapFunFR[T, U](l: List[T], f: T => U) = {
    (l foldRight List[U]()) ((x, r) => f(x) :: r)
  }

  def mapFunFL[T, U](l: List[T], f: T => U) = {
    (l foldLeft List[U]()) ((r, x) => r :+ f(x))
  }

  def lengthFunFR[T](list: List[T]): Int = {
    (list foldRight 0) ((x, r) => 1 + r)
  }

  def lengthFunFL[T](list: List[T]): Int = {
    (list foldRight 0) ((r, x) => 1 + x)
  }

  def add(a: Int)(b: Int) = a + b
}