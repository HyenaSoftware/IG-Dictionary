package com.hyenawarrior

import java.lang.String.{format => _}

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{VerbModeEnum, VerbTenseEnum, VerbVoice, WeakVerbClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.WeakVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbType, WeakVerb}
import com.hyenawarrior.auxiliary.getCauses
import org.junit.Assert._
import org.junit.Assert

/**
  * Created by HyenaWarrior on 2018.04.15..
  */
object VerbTestAux {

  private val ABBREVATIONS_OF = Map[Any, String](

    SG_1 -> "SG1",
    SG_2 -> "SG2",
    SG_3 -> "SG3",

    PL_1 -> "PL1",
    PL_2 -> "PL2",
    PL_3 -> "PL3",

    // Verb tenses
    PRESENT -> "PRS",
    PAST    -> "PST",

    // Voices
    ACTIVE        -> "ACT",
    MEDIO_PASSIVE -> "MID-PAS",

    // Moods
    INFINITIVE  -> "INF",
    INDICATIVE  -> "IND",
    SUBJUNCTIVE -> "SBJV",
    IMPERATIVE  -> "IMP",
    PARTICIPLE  -> "PTCP",

    // numbers
    SINGULAR -> "SG",
    PLURAL   -> "PL",

    // cases
    NOMINATIVE -> "NOM",
    ACCUSATIVE -> "ACC",
    DATIVE     -> "DAT",
    GENITIVE   -> "GEN"
  )

  def abbrevationOf[T](obj: T): String = obj match {

    case ((number: GNumber, caze: Case), definite: Boolean) =>
      abbrevationOf(number) + " " + abbrevationOf(caze) + (if(definite) " DEF" else "")

    case (mood: VerbModeEnum, voice: VerbVoice, optTense: Option[VerbTenseEnum], optPronoun: Option[Pronoun]) =>

      val md = Some(abbrevationOf(mood))
      val vc = Some(abbrevationOf(voice))
      val ts = optTense.map(abbrevationOf)
      val pr = optPronoun.map(abbrevationOf)

      Seq(ts, md, pr, vc).flatten.mkString(" ")

    case _ => ABBREVATIONS_OF getOrElse(obj, obj.toString)
  }

  trait Status
  case class StemStatus(stemType: EnumVerbStem, stat: Either[Exception, (String, String)]) extends Status
  case class FormStatus(stat: Either[Exception, Iterable[(VerbType, String, String)]]) extends Status
  case class VerbGenerationError(e: Exception) extends Status

  object FormStatus {

    def apply(e: Exception) = new FormStatus(Left(e))
    def apply(iterable: Iterable[(VerbType, String, String)]) = new FormStatus(Right(iterable))
  }

  def diff(stemClass: WeakVerbClassEnum, stems: Map[EnumVerbStem, String], forms: Map[VerbType, String]): Unit = {

    val countOfTests = forms.size

    val results = for(((srcForm, strForm), idx) <- forms.zipWithIndex) yield {

      val tableName = s"\nGenerated forms from $strForm [${abbrevationOf(srcForm)}] (${idx+1} of $countOfTests):"

      val status = try {

        val verb = WeakVerb(stemClass, Map(srcForm -> strForm))

        val generatedStem = (verb forms srcForm).getStem

        checkStem(stems, generatedStem) getOrElse checkForms(forms, srcForm, verb)

      } catch { case e: Exception => VerbGenerationError(e) }

      tableName -> status
    }

    printDiffs(results)
  }

  def checkForms(forms: Map[VerbType, String], srcForm: VerbType, verb: WeakVerb): FormStatus = try FormStatus {

    val expectedForms = forms - srcForm

    val items = for ((expectedFormType, expectedStrForm) <- expectedForms) yield {

      val generatedForm = verb forms expectedFormType

      (expectedFormType, expectedStrForm, generatedForm.strRepr)
    }

    items.filter { case (_, a, b) => a != b }
  } catch { case e: Exception => FormStatus(e) }

  def checkStem(stems: Map[EnumVerbStem, String], generatedStem: WeakVerbStem): Option[StemStatus] = try {

    val expectedStem = stems(generatedStem.stemType)

    if (expectedStem == generatedStem.stem) None else Some {

      StemStatus(generatedStem.stemType, Right((expectedStem, generatedStem.stem)))
    }
  } catch { case e: Exception => Some(StemStatus(generatedStem.stemType, Left(e))) }

  def printDiffs(differences: Map[String, Status]): Unit = {

    if(differences.nonEmpty) {

      val diffText = differences
        .map {
          case (tableName, VerbGenerationError(exception))        => getCauses(exception).mkString(s"$tableName  (failed to generate the verb)\n","\n  caused by: ", "")
          case (tableName, StemStatus(stemType, Left(exception))) => getCauses(exception).mkString(s"$tableName  (stem doesn't match)\n","\n  caused by: ", "")
          case (tableName, StemStatus(stemType, Right((expectedStem, givenStem)))) =>

            val abbrevation = stemType.name

            val w0 = Seq(abbrevation, "Stem").map(_.length).max
            val w1 = Seq(expectedStem, "Expected").map(_.length).max
            val w2 = Seq(givenStem, "Generated").map(_.length).max

            val FORMAT_STR_HDR = s" %-${w0}s | %-${w1}s | %-${w2}s"
            val FORMAT_STR_RCD = s" %-${w0}s | %-${w1}s ~ %-${w2}s"

            val header = String.format(FORMAT_STR_HDR, "Stem", "Expected", "Generated")
            val record = String.format(FORMAT_STR_RCD, abbrevation, expectedStem, givenStem)

            s"""$tableName
               |$header
               |$record
             """.stripMargin

          case (tableName, FormStatus(Left(exception))) => getCauses(exception).mkString(s"$tableName  (verb form is missing)\n","\n  caused by: ", "")
          case (tableName, FormStatus(Right(records))) =>

            val abbrevatedRecords = records.map { case (vt, e2, e3) => (abbrevationOf(vt), e2, e3) }

            val w0 = (abbrevatedRecords.map(_._1.length).toSeq :+ "Conjugation".length).max
            val w1 = (abbrevatedRecords.map(_._2.length).toSeq :+ "Expected".length).max
            val w2 = (abbrevatedRecords.map(_._3.length).toSeq :+ "Generated".length).max

            val header = String.format(s" %-${w0}s | %-${w1}s | %-${w2}s", "Conjugation", "Expected", "Generated")

            abbrevatedRecords
              .map { case (ntype, e2, e3) => String.format(s" %-${w0}s | %-${w1}s ~ %-${w2}s", ntype, e2, e3)}
              .mkString(s"$tableName\n$header\n", "\n", "")
        }
        .mkString("\n")

      fail(diffText)
    }
  }
}
