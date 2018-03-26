package com.hyenawarrior

import java.lang.String._

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.auxiliary.getCauses
import org.junit.Assert._

import scala.language.implicitConversions

/**
  * Created by HyenaWarrior on 2018.02.03..
  */
object NounTestAux {

  case class Form(str: String, reversible: Boolean)

  implicit def regular(str: String): Form = Form(str, true)

  def nonReversible(str: String) = Form(str, false)

  private def abbrevationOf(decl: NounType, isDef: Boolean): String = (if(isDef) "DEF " else "INDEF ") +
    abbrevationOf(decl._1) + " " + abbrevationOf(decl._2)

  private def abbrevationOf(caze: Case): String = caze match {

    case NOMINATIVE => "NOM"
    case ACCUSATIVE => "ACC"
    case DATIVE => "DAT"
    case GENITIVE => "GEN"
  }

  private def abbrevationOf(number: GNumber): String = number match {

    case SINGULAR => "SG"
    case PLURAL => "PL"
  }

  def diff(stemClass: NounStemClass, forms: Map[NounType, Form], definiteForms: Map[NounType, Form]): Unit = {

    val allForms = forms.map { case (k, v) => (k, false) -> v } ++ definiteForms.map { case (k, v) => (k, true) -> v }

    diff(stemClass, allForms)
  }

  def diff(stemClass: NounStemClass, forms: Map[NounFormType, Form]): Unit = {

    val countOfTests = forms.size
    val differences = forms
      .zipWithIndex
      .collect { case ((formType @ (decl, isDef), Form(str, true)), idx) =>
        val tableName = s"\nGenerated forms from $str [${abbrevationOf(decl, isDef)}] (${idx+1} of $countOfTests):"
        (tableName, generateTheseFrom(stemClass, formType -> str, forms - formType))

      }.filter {
        case (_, Left(exception)) => true
        case (_, Right(result)) => result.nonEmpty
      }

    if(differences.nonEmpty) {

      val diffText = differences
        .map {
          case (tableName, Left(exception)) => getCauses(exception).mkString(s"$tableName  \n","\n  caused by: ", "")
          case (tableName, Right(records)) =>

            val abbrevatedRecords = records.map { case ((decl, isDef), e2, e3) => (abbrevationOf(decl, isDef), e2, e3) }

            val w0 = (abbrevatedRecords.map(_._1.length).toSeq :+ "Declension".length).max
            val w1 = (abbrevatedRecords.map(_._2.length).toSeq :+ "Expected".length).max
            val w2 = (abbrevatedRecords.map(_._3.length).toSeq :+ "Generated".length).max

            val header = format(s" %-${w0}s | %-${w1}s | %-${w2}s", "Declension", "Expected", "Generated")

            abbrevatedRecords
              .map { case (ntype, e2, e3) => format(s" %-${w0}s | %-${w1}s ~ %-${w2}s", ntype, e2, e3)}
              .mkString(s"$tableName\n$header\n", "\n", "")
        }
        .mkString("\n")

      fail(diffText)
    }
  }

  private def generateTheseFrom(stemClass: NounStemClass, givenForm: (NounFormType, String)
    , expectedForms: Map[NounFormType, Form]): Either[Exception, Iterable[(NounFormType, String, String)]] = try {

    val generatedForms = Noun(stemClass, Map(givenForm)).nounForms

    val result = expectedForms.map {
      case (nt, Form(expStr, _)) => generatedForms.get(nt)
        .map(nf => (expStr == nf.strRepr, nt, expStr, nf.strRepr))
        .getOrElse((false, nt, expStr, "<missing>"))

    }.collect {
      case (false, nt, expStr, gvnStr) => (nt, expStr, gvnStr)
    }

    Right(result)

  } catch {
    case e: RuntimeException => Left(e)
  }
}
