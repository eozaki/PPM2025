package Aula04

import Utils.Test.assertEquals;

object Exercicios {
  // Exercício 1
  def concatListsR[T](lists: List[List[T]]): List[T] =
    (lists foldRight List[T]()) (_ ++ _)

  def chainedAndR(list: List[Boolean]): Boolean =
    (list foldRight true) ((x, r) => x && r)

  def chainedOrR(list: List[Boolean]): Boolean =
    (list foldRight false) ((x, r) => x || r)

  def concatListsL[T](lists: List[List[T]]): List[T] =
    (lists foldLeft List[T]()) (_ ++ _)

  def chainedAndL(list: List[Boolean]): Boolean =
    (list foldLeft true) ((r, x) => x && r)

  def chainedOrL(list: List[Boolean]): Boolean =
    (list foldLeft false) ((r, x) => x || r)

  def remDup[T](list: List[T]): List[T] =
    (list foldRight List[T]()) ((x, r) => x :: r.dropWhile((y: T) => { y == x }))

  // Exercício 2
  type Team = String
  type Goals = Int
  type Match = ((Team, Goals), (Team, Goals))
  type Fixtures = List[Match]

  def noItself(matches: Fixtures): Boolean =
    (matches foldRight true) ((m, r) => m._1._1 != m._2._1 && r)

  def withoutRep(matches: Fixtures): (Boolean, List[Team]) = {
    val l: List[Team] = teams(matches)
    (l foldRight (true, List[Team]())) ((t, r) => (!r._2.exists(x => x == t) && r._1, t :: r._2))
  }

  def teams(matches: Fixtures): List[Team] =
    (matches foldRight List[Team]()) ((x, r) => x._1._1 :: x._2._1 :: r)

  def main(args: Array[String]): Unit = {
    assertEquals(concatListsR(List(List(1, 2), List(3, 4))), List(1, 2, 3, 4))
    assertEquals(concatListsL(List(List(1, 2), List(3, 4))), List(1, 2, 3, 4))

    assertEquals(chainedAndL(List(true, true)), true)
    assertEquals(chainedAndL(List(true, false)), false)
    assertEquals(chainedOrL(List(true, false)), true)
    assertEquals(chainedOrL(List(false, false)), false)

    assertEquals(chainedAndR(List(true, true)), true)
    assertEquals(chainedAndR(List(true, false)), false)
    assertEquals(chainedOrR(List(true, false)), true)
    assertEquals(chainedOrR(List(false, false)), false)

    assertEquals(remDup(List(1,2,3,3)), List(1,2,3))

    val t1: Team = "Banana"
    val t2: Team = "dePijama"
    val g1: Goals = 1
    val g2: Goals = 2

    val m1: Fixtures = List(((t1, g1), (t2, g2)))
    val m2: Fixtures = List(((t1, g1), (t1, g2)))

    assertEquals(noItself(m1), true)
    assertEquals(noItself(m2), false)

    assertEquals(teams(m1), List(t1, t2))
    assertEquals(teams(m2), List(t1, t1))

    assertEquals(withoutRep(m1)._1, true)
  }
}
