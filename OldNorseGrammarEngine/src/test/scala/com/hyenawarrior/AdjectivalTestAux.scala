package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.{Adjective, AdjectiveStem}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.{FEMININE, MASCULINE, NEUTER}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{apply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{apply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{VerbModeEnum, VerbTenseEnum, VerbVoice}
import com.hyenawarrior.auxiliary.getCauses
import org.junit.Assert._

import scala.collection.immutable.ListMap

/**
  * Created by HyenaWarrior on 2018.06.12..
  */
object AdjectivalTestAux {

  private val ABBREVATIONS_OF = Map[Any, String](

    //
    POSITIVE_INDEFINITE -> "INDF",
    POSITIVE_DEFINITE -> "DEF",
    COMPARATIVE -> "COMP",
    SUPERLATIVE_INDEFINITE -> "SUPL INDEF",
    SUPERLATIVE_DEFINITE -> "SUPL DEF",
    DETERMINERS -> "DET",

    // numbers
    SINGULAR -> "SG",
    PLURAL   -> "PL",

    // genders
    MASCULINE -> "M",
    FEMININE -> "F",
    NEUTER -> "N",

    // cases
    NOMINATIVE -> "NOM",
    ACCUSATIVE -> "ACC",
    DATIVE     -> "DAT",
    GENITIVE   -> "GEN"
  )

  trait Status
  case class StemStatus(stat: Either[Exception, (String, String)]) extends Status
  case class FormStatus(stat: Either[Exception, Iterable[(AdjectiveFormType, String, String)]]) extends Status
  case class AdjectiveGenerationError(e: Exception) extends Status

  object FormStatus {

    def apply(e: Exception) = new FormStatus(Left(e))
    def apply(iterable: Iterable[(AdjectiveFormType, String, String)]) = new FormStatus(Right(iterable))
  }

  def abbrevationOf[T](obj: T): String = obj match {

    case ((number: GNumber, caze: Case), definite: Boolean) =>
      abbrevationOf(number) + " " + abbrevationOf(caze) + (if(definite) " DEF" else "")

    case (mood: VerbModeEnum, voice: VerbVoice, optTense: Option[VerbTenseEnum], optPronoun: Option[Pronoun]) =>

      val md = Some(abbrevationOf(mood))
      val vc = Some(abbrevationOf(voice))
      val ts = optTense.map(abbrevationOf)
      val pr = optPronoun.map(abbrevationOf)

      Seq(ts, md, pr, vc).flatten.mkString(" ")

    case (adjType: AdjectiveType, number: GNumber, gender: Gender, caze: Case) =>
      abbrevationOf(adjType) + " " + abbrevationOf(number) + " " + abbrevationOf(gender) + " " + abbrevationOf(caze)

    case _ => ABBREVATIONS_OF getOrElse(obj, obj.toString)
  }

  def diff(expectedStem: String, strongForms: Array[String], weakForms: Array[String]): Unit = {

    def toFormTypes(forms: Array[String], adjType: AdjectiveType) = GNumber.conventionalValues
        .flatMap(n => Case.values
          .flatMap(c => Gender.values
            .map(g => fromTuple(adjType, n, g, c))))
      .zipWithIndex.map { case (f, i) => f -> forms(i)}
      .foldLeft(ListMap[AdjectiveFormType, String]()){ case (m, e) => m + e }

    diff(expectedStem, toFormTypes(strongForms, POSITIVE_INDEFINITE)) // ++ toFormTypes(weakForms, POSITIVE_DEFINITE))
  }

  def diff(expectedStem: String, forms: Map[AdjectiveFormType, String]): Unit = {

    val countOfTests = forms.size

    val results = for(((srcForm, strForm), idx) <- forms.zipWithIndex) yield {

      val optStatus: Option[Status] = try {

        val adjective = Adjective.from(Map(srcForm -> strForm))

        checkStem(expectedStem, adjective.stem) orElse checkForms(forms, srcForm, adjective)

      } catch {

        case e: Exception => Some(AdjectiveGenerationError(e))
      }

      val tableName = s"\nGenerated forms from '$strForm' [${abbrevationOf(srcForm)}] ($expectedStem-) (${idx+1} of $countOfTests):"

      tableName -> optStatus
    }

    printDiffs(results.collect { case(tn, Some(st)) => tn -> st })
  }

  def checkStem(expectedStem: String, generatedStem: AdjectiveStem): Option[StemStatus] = try {

    if (expectedStem == generatedStem.stemStr) None else Some {

      StemStatus(Right((expectedStem, generatedStem.stemStr)))
    }
  } catch { case e: Exception => Some(StemStatus(Left(e))) }

  def checkForms(forms: Map[AdjectiveFormType, String], srcForm: AdjectiveFormType, verb: Adjective): Option[FormStatus] = try {

    val expectedForms = forms - srcForm

    val items = for ((expectedFormType, expectedStrForm) <- expectedForms) yield {

      val generatedForm = verb forms expectedFormType

      (expectedFormType, expectedStrForm, generatedForm.strRepr)
    }

    items.filter { case (_, a, b) => a != b } match {

      case List() => None
      case list => Some(FormStatus(list))
    }

  } catch { case e: Exception => Some(FormStatus(e)) }

  def printDiffs(differences: Map[String, Status]): Unit = {

    if(differences.nonEmpty) {

      val diffText = differences
        .map {
          case (tableName, AdjectiveGenerationError(exception))        => getCauses(exception).mkString(s"$tableName  (failed to generate the adjective)\n","\n  caused by: ", "")
          case (tableName, StemStatus(Left(exception))) => getCauses(exception).mkString(s"$tableName  (stem doesn't match)\n","\n  caused by: ", "")
          case (tableName, StemStatus(Right((expectedStem, givenStem)))) =>

            val w0 = Seq(expectedStem, "Expected").map(_.length).max
            val w1 = Seq(givenStem, "Generated").map(_.length).max

            val FORMAT_STR_HDR = s" %-${w0}s | %-${w1}s"
            val FORMAT_STR_RCD = s" %-${w0}s ~ %-${w1}s"

            val header = String.format(FORMAT_STR_HDR, "Expected", "Generated")
            val separator = "=" * header.length
            val record = String.format(FORMAT_STR_RCD, expectedStem, givenStem)

            s"""$tableName
               |$header
               |$separator
               |$record
               """.stripMargin

          case (tableName, FormStatus(Left(exception))) => s"$tableName  (adjective form is missing)  ${exception.getMessage}"
          case (tableName, FormStatus(Right(records))) =>

            val abbrevatedRecords = records.map { case (vt, e2, e3) => (abbrevationOf(vt), e2, e3) }.toSeq

            val w0 = (abbrevatedRecords.map(_._1.length).toSeq :+ "Declension".length).max
            val w1 = (abbrevatedRecords.map(_._2.length).toSeq :+ "Expected".length).max
            val w2 = (abbrevatedRecords.map(_._3.length).toSeq :+ "Generated".length).max

            val header = String.format(s" %-${w0}s | %-${w1}s | %-${w2}s (${abbrevatedRecords.size} total(s))", "Declension", "Expected", "Generated")
            val separator = "=" * header.length

            abbrevatedRecords
              .map { case (ntype, e2, e3) => String.format(s" %-${w0}s | %-${w1}s ~ %-${w2}s", ntype, e2, e3)}
              .mkString(s"$tableName\n$header\n $separator\n", "\n", "")
        }
        .mkString("\n")

      fail(diffText)
    }
  }
}
