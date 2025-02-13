package Aula01

object Fun {
  def func1(x: Double, y: Int) = x + (70 * y)

  def ex(a: Double) = 50 * a

  def ex2a(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1 + a._2, b._2 * b._1)

  def ex2b(a: Int, b: Int, c: Int) = {
    if(a >= b && a >= c) {
      if(b > c) (a, b)
      else (a, c)
    } else if(b >= a && b >= c) {
      if(a > c) (b, a)
      else (b, c)
    } else {
      if(a > b) (c, a)
      else (c, b)
    }
  }

  def ex2c(nums: (Int, Int, Int)): (Int, Int, Int) = {
    val a = nums._1
    val b = nums._2
    val c = nums._3

    if(a >= b && a >= c) {
      if(b > c) (a, b, c)
      else (a, c, b)
    } else if(b >= a && b >= c) {
      if(a > c) (b, a, c)
      else (b, c, a)
    } else {
      if(a > b) (c, a, b)
      else (c, b, a)
    }
  }

  def ex2d(a: Int, b: Int, c: Int): Boolean = a + b > c && a + c > b && b + c > a

  def ex2e(name: String): String = {
    val names = name.split(" ")
    return names.head + " " + names.last
  }

  def ex2eRec(name: List[String]): String = {
    name.head + " " + lastName(name.tail)
  }

  def lastName(list: List[String]): String = {
    if(list.length == 1) {
      list.head
    } else {
      lastName(list.tail)
    }
  }

  def ex3a(x: Int, y: Int): Int = if(y == 1) return x; else return x * ex3a(x, y - 1)

  def ex3b(list: List[Int]): (Int, Int) = {
    (list.head, list.last)
  }

  def ex3c(list: List[Int]): (List[Int], Int) = return (list, list.length)

  def ex3d(list: List[Double]): Double = {
    if (list.isEmpty) {
      0.0
    } else {
      return sum(list) / list.length
    }
  }

  def sum(list: List[Double]): Double = {
    if(list.isEmpty) {
      0.0
    } else if(list.size == 1) {
      list.head
    } else {
      list.head + sum(list.tail)
    }
  }
}
