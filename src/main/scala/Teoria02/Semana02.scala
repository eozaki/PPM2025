package Teoria02

object Semana02 {
  def isPal[E](x: List[E]): Boolean = x match {
    case List() => true
    case List(x) => true
    case x :: xs => x == xs.last && isPal(xs.init)
  }
}
