package Aula05.Ex2

import Aula05.Ex2.Turma._
import Aula05.Ex2.RegimeOPT.RegimeOPT

import scala.collection.View.Empty

case class Turma(id: String, alunos: Alunos){
  def trabs() = Turma.trabs(this)
  def searchStudent(a: List[Turma.Aluno], n: Int) = Turma.searchStudent(a, n)
}

object Turma {
  type Nome = String
  type Numero = Int
  type NT = Option[Float]
  type NP = Option[Float]
  type Regime = RegimeOPT
  type Aluno = (Numero, Nome, Regime, NT, NP)
  type Alunos = List[Aluno]

  def trabs(t: Turma): Alunos = t.alunos match {
    case List() => List()
    case a :: as => a match {
      case (_, _, RegimeOPT.TrabEstud, _, _) => a :: trabs(Turma(t.id, as))
      case _ => trabs(Turma(t.id, as))
    }
  }

  def searchStudent(as: Alunos, n: Numero): Option[Aluno] = as match {
    case List() => None
    case x :: _xs if x._1 == n => Some(x)
    case _ :: xs => searchStudent(xs, n)
  }

  def finalGrade(as: Alunos, n: Numero): Option[Double] = {
    val aluno = searchStudent(as, n).get

    if(aluno != None && aluno._4 != None && aluno._5 != None) {
      val nt = aluno._4.get
      val np = aluno._5.get

      if(nt >= 9.5 && np >= 9.5) Some(0.4 * nt + 0.6 * np)
    }

    None
  }
}
