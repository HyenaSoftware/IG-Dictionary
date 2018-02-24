package com.hyenawarrior

import java.lang.String._

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import org.junit.Assert._

import scala.language.implicitConversions

/**
  * Created by HyenaWarrior on 2018.02.03..
  */
object NounTestAux {

  case class Form(str: String, reversible: Boolean)

  implicit def regular(str: String): Form = Form(str, true)

  def nonReversible(str: String) = Form(str, false)

  private def abbrevationOf(decl: NounType): String = abbrevationOf(decl._1) + " " + abbrevationOf(decl._2)

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

  def diff(stemClass: NounStemClass, forms: Map[NounType, Form]): Unit = {

    val countOfTests = forms.size
    val differences = forms
      .zipWithIndex
      .collect { case ((decl, Form(str, true)), idx) =>
        val tableName = s"\nGenerated forms from $str [${abbrevationOf(decl)}] (${idx+1} of $countOfTests):"
        (tableName, generateTheseFrom(stemClass, decl -> str, forms - decl))

      }.filter {
        case (_, Left(exception)) => true
        case (_, Right(result)) => result.nonEmpty
      }

    if(differences.nonEmpty) {

      val diffText = differences
        .map {
          case (tableName, Left(exception)) => s"$tableName\n${exception.getMessage}"
          case (tableName, Right(records)) =>
            val w0 = "Declension".length
            val w1 = (records.map(_._2.length).toSeq :+ "Expected".length).max
            val w2 = (records.map(_._3.length).toSeq :+ "Generated".length).max

            val header = format(s" %-${w0}s | %-${w1}s | %-${w2}s", "Declension", "Expected", "Generated")

            records
              .map(e => format(s" %-${w0}s | %-${w1}s ~ %-${w2}s", abbrevationOf(e._1), e._2, e._3))
              .mkString(s"$tableName\n$header\n", "\n", "")
        }
        .mkString("\n")

      fail(diffText)
    }
  }

  private def generateTheseFrom(stemClass: NounStemClass, givenForm: (NounType, String)
    , expectedForms: Map[NounType, Form]): Either[Exception, Iterable[(NounType, String, String)]] = try {

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
