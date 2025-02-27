package Teoria03

import scala.annotation.tailrec

object Semana03 {
  def maybeTwice(b: Boolean, i: => Int) =
    if(b) i + i else 0

  def maybeTwice2(b: Boolean, i: => Int) = {
    val y = i;
    if(b) y + y else 0
  }

  def main(args: Array[String]) = {
    Semana03.maybeTwice(true, { println("repete"); 41 + 1 })

    Semana03.maybeTwice2(true, { println("não repete"); 41 + 1 })
  }

  def expr = {
    val x = { print("x"); 1 } // Imprime na atribuição
    lazy val y = { print("y"); 2 } // Imprime só na primeira chamada
    def z = { print("z"); 3 } // Imprime só quando é convocado

    z + y + x + z + y + x // => xzyz
  }

  def sum(f: Int => Int, a: Int, b: Int): Int = { // De a até b
    if(a >= b) f(b)
    else f(a) + sum(f, a + 1, b)
  }

  def op(f: Int => Int, g: (Int, Int) => Int, a: Int, b: Int): Int = {
    if(a >= b) f(b)
    else g(f(a), op(f, g, a + 1, b))
  }

  def cube(x: Int) = x * x * x

  def squareList(x: List[Int]): List[Int] = {
    x map (y => y * y)
  }

  def tailSum(f: Int => Int, a: Int, b: Int): Int ={
    @tailrec
    def loop(a1: Int, acc: Int): Int = {
      if(a1 >= b) f(b) + acc
      else loop(a + 1, acc + f(a1))
    }

    loop(a, 0)
  }
}

object ExpensiveResource {
  lazy val resource: Int = init()

  def init(): Int = {
    // Do something
    0
  }
}
