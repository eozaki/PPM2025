package Aula02

object Semana02 {
  def transf[A](list: List[A]): List[A] = list match {
    case List() => list
    case _ :: List() => list
    case x1 :: x2 :: xs => x2 :: x1 :: transf(xs)
  }

  def product(list: List[Int]): Double = list match {
    case List() => 0
    case x :: List() => x
    case 0 :: xs => 0
    case x :: xs => x * product(xs)
  }

  def addToTail[E](x: E, list: List[E]): List[E] = list match {
    case List() => List(x)
    case head :: xs => head :: addToTail(x, xs)
  }

  def concatLists[E](l1: List[E], l2: List[E]): List[E] = (l1, l2) match {
    case (List(), List()) => List()
    case (x :: xs, List()) => concatLists(l2, l1)
    case (List(), x :: xs) => x :: concatLists(l1, xs)
    case (x1 :: xs1, _) => x1 :: concatLists(xs1, l2)
  }

  def sumEl(list: List[(Int, Int)], index: Int = 0): Int = {
    (list, index) match {
      case (List(), _) => 0
      case (x :: xs, 2) => x._1 + x._2 + sumEl(xs, index + 1)
      case (x :: List(), 2) => x._1 + x._2
      case (x :: _, 4) => x._1 + x._2
      case (_ :: xs, _) => sumEl(xs, index + 1)
    }
  }

  def lengthAndSum(list: List[Double]): (Int, Double) = list match {
    case List() => (0, 0)
    case x :: List() => (1, x)
    case x :: xs => {
      val l = lengthAndSum(xs)
      (1 + l._1, x + l._2)
    }
  }

  def avg(list: List[Double]): Double = {
    val result = lengthAndSum(list)
    result match {
      case (_, 0) => 0
      case (i, d) => d / i
    }
  }

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

  def aboveAvg(list: List[Double]): List[Double] = metH(list, avg(list))._2
}
