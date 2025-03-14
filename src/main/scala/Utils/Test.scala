package Utils

object Test {
  def assertEquals(tested: Any, target: Any): Boolean = tested.equals(target) match {
    case true => {
      println("Tudo certo!")
      true
    }
    case false => {
      println("Algo de errado não está certo...")
      false
    }
  }
}
