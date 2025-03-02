package Aula03

import scala.annotation.tailrec

object Aula03 {
  // ex 1.1
  def fatNoIf(n: Int): Int = n match {
    case 0 => 1
    case _ => fatNoIf(n - 1) * n
  }

  def fatRec(n: Int): Int = {
    if(n == 0) 1
    else n * fatRec(n - 1)
  }

  @tailrec
  def fatTailRec(n: Int, acc: Int = 1): Int = {
    if(n == 0) acc
    else fatTailRec(n - 1, acc * n)
  }

  // ex 1.2
  def remDup1[E](list: List[E]): List[E] = list match {
    case List() => List()
    case x1 :: xs => x1 :: remDup1(xs.dropWhile( x => { x == x1 }))
  }

  @tailrec
  def remDup2[E](list: List[E], acc: List[E] = List()): List[E] = list match {
    case List() => acc
    case x1 :: xs => remDup2(xs.dropWhile((x: E) => { x == x1 }), acc :+ x1)
  }

  @tailrec
  def remDup3[E](list: List[E], acc: List[E] = List()): List[E] = list match {
    case List() => acc
    case List(x) => acc :+ x
    case x1 :: x2 :: tail => if (x1 == x2) remDup3(x2 :: tail, acc)
    else remDup3(x2 :: tail, acc :+ x1)
  }

  // Ex 2
  def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
    println(lo)
    if(lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo + 1, hi)
  }

  def listRange(lo: Int, hi: Int): List[Int] = {
    println(lo)
    if(lo >= hi) List()
    else lo :: listRange(lo+1, hi)
  }

  // Ex 3.a
  def addLists(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (Nil, Nil) => List()
    case (x :: xs, y :: ys) => x + y :: addLists(xs, ys)
  }

  // Ex 3.b
  def zipWith[E](l1: List[E], l2: List[E], f: (E, E) => E): List[E] = (l1, l2) match {
    case (List(), List()) => List()
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys, f)
  }

  // Ex 3.c
  def isSorted[E](list: List[E], comparison: (E, E) => Boolean): Boolean = list match {
    case List() => true
    case List(_) => true
    case(x :: y :: xs) => comparison(x, y) && isSorted(y :: xs, comparison)
  }

  // Ex 3.d
  def bubbleSort[E](list: List[E], comparison: (E, E) => Boolean): List[E] = list match {
    case List() => List()
    case List(_) => list
    case x :: y :: tail => {
      if (comparison(x, y)) x :: bubbleSort(y :: tail, comparison)
      else bubbleSort(y :: bubbleSort(x :: tail, comparison), comparison)
    }
  }

  def main(args: Array[String]): Unit = {
//    println(remDup1(List('a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e')))
//    println(remDup2(List('a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e')))
//    println(remDup3(List('a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e')))
//
//    listRange(1, 5).take(3).toList
//    lazyListRange(1, 5).take(3).toList
//    println(zipWith(List(1,2,3,4), List(2,3,4,5), (x: Int, y: Int) => x * y))
//
//    println(isSorted(List(), (x: Int, y: Int) => x <= y))
//    println(isSorted(List(1, 2, 3, 4, 5), (x: Int, y: Int) => x <= y))
//    println(isSorted(List(1, 2, 34, 5, 6), (x: Int, y: Int) => x <= y))
    println(bubbleSort(List(3, 4, 2, 1), (x: Int, y: Int) => x <= y))
  }
}

