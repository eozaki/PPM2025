package Aula02

import scala.annotation.tailrec

object Semana02 {

  // 1.a
  def transf[A](list: List[A]): List[A] = list match {
    case List() => list
    case _ :: List() => list
    case x1 :: x2 :: xs => x2 :: x1 :: transf(xs)
  }

  // 1.b
  def product(list: List[Int]): Double = list match {
    case List() => 0
    case x :: List() => x
    case 0 :: xs => 0
    case x :: xs => x * product(xs)
  }

  // 1.c
  def addToTail[E](x: E, list: List[E]): List[E] = list match {
    case List() => List(x)
    case head :: xs => head :: addToTail(x, xs)
  }

  // 1.d
  def concatLists[E](l1: List[E], l2: List[E]): List[E] = (l1, l2) match {
    case (List(), List()) => List()
    case (x :: xs, List()) => concatLists(l2, l1)
    case (List(), x :: xs) => x :: concatLists(l1, xs)
    case (x1 :: xs1, _) => x1 :: concatLists(xs1, l2)
  }

  // 1.e
  def sumEl(list: List[(Int, Int)], index: Int = 0): Int = {
    (list, index) match {
      case (List(), _) => 0
      case (x :: xs, 2) => x._1 + x._2 + sumEl(xs, index + 1)
      case (x :: List(), 2) => x._1 + x._2
      case (x :: _, 4) => x._1 + x._2
      case (_ :: xs, _) => sumEl(xs, index + 1)
    }
  }

  // 1.f1
  def lengthAndSum(list: List[Double]): (Int, Double) = list match {
    case List() => (0, 0)
    case x :: List() => (1, x)
    case x :: xs => {
      val l = lengthAndSum(xs)
      (1 + l._1, x + l._2)
    }
  }

  // 1.f2
  def avg(list: List[Double]): Double = {
    val result = lengthAndSum(list)
    result match {
      case (_, 0) => 0
      case (i, d) => d / i
    }
  }

  // 1.g
  def metH(list: List[Double], limit: Double): (List[Double], List[Double]) = list match {
    case List() => (List(), List())
    case h :: xs if h < limit => {
      val res = metH(xs, limit)
      (h :: res._1, res._2)
    }
    case h :: xs => {
      val res = metH(xs, limit)
      (res._1, h :: res._2)
    }
  }

  // 1.h
  def aboveAvg(list: List[Double]): List[Double] = metH(list, avg(list))._2
}

object PhoneBook {
  // BEGIN - CODE PROVIDED BY PROFESSORS
  type Entry = (String, String, String)
              // name, phone number, email address
  type LTelef = List[Entry]

  def emails(lst : LTelef) : List[String] = lst match {
    case Nil => Nil
    case (_ , _ , email):: tail => email :: (emails(tail))
  }
  // END - CODE PROVIDED BY PROFESSORS

  def landlineEmails(list: LTelef): List[String] = list match {
    case List() => List()
    case (_, phone, email) :: xs => {
      phone.charAt(0) match {
        case '2' => email :: landlineEmails(xs)
        case _ => landlineEmails(xs)
      }
    }
  }

  @tailrec
  def findByPhoneAndName(list: LTelef, phone: String, name: String): Entry = list match {
    case List() => ("", "", "")
    case (n, p, e) :: xs => (p, n) match {
      case (phone, name) => (name, phone, e)
      case (_, _) => findByPhoneAndName(xs, phone, name)
    }
  }

  def main(args: Array[String]): Unit = {
    var contatos = List(("Erick", "910986347", "ecoio@iscte-iul.pt"), ("Erick", "210986347", "ecoio@usp"))

    Aula02.PhoneBook.landlineEmails(contatos)
    Aula02.PhoneBook.findByPhoneAndName(contatos, "910986347", "Erick")
  }
}
