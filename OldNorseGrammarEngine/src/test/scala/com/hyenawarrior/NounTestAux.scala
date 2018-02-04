package com.hyenawarrior

import java.lang.String._

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, _}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import org.junit.Assert._

/**
  * Created by HyenaWarrior on 2018.02.03..
  */
object NounTestAux {

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

  def diff(stemClass: NounStemClass, forms: Map[NounType, String]): Unit = {

    val countOfTests = forms.size
    val differences = forms
      .zipWithIndex
      .map { case (base, idx) =>
        val tableName = s"\nGenerated forms from ${base._2} [${abbrevationOf(base._1)}] (${idx+1} of $countOfTests):"
        (tableName, generateCertainForm(stemClass, base, forms - base._1))

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

  private def generateCertainForm(stemClass: NounStemClass, givenForm: (NounType, String)
    , expectedForms: Map[NounType, String]): Either[Exception, Iterable[(NounType, String, String)]] = try {

    val generatedForms = Noun(stemClass, Map(givenForm)).nounForms

    val result = expectedForms.map {
      case (nt, expStr) => generatedForms.get(nt)
        .map(nf => (expStr == nf.strRepr, nt, expStr, nf.strRepr))
        .getOrElse((false, nt, expStr, "<missing>"))

    }.collect {
      case e@(false, nt, expStr, gvnStr) => (nt, expStr, gvnStr)
    }

    Right(result)

  } catch {
    case e: RuntimeException => Left(e)
  }
}
