package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.FinitiveStrongVerbForm.tenseAndNumberToStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveStrongVerbForm.{moodAndTenseToStem, toNonFiniteVerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.persister.{Reader, Serializer}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

import scala.language.postfixOps

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
package object serializers {

  implicit val ALL_SERIALIZER: Map[Class[_], Serializer[_]] = Map(
    classOf[StrongVerb] -> StrongVerbContextMarshaller,
    classOf[DictionaryEntry] -> DictionaryEntryMarshaller,
    classOf[MeaningDef] -> MeaningDefMarshaller)

  implicit object AblautGradeOrdering extends Ordering[(EnumVerbStem, AblautGrade)] {

    override def compare(x: (EnumVerbStem, AblautGrade), y: (EnumVerbStem, AblautGrade)): Int = {

      val xId = EnumVerbStem.idOf(x._1)
      val yId = EnumVerbStem.idOf(y._1)
      val dId = yId - xId
      if(dId == 0) x._2.rootVowel.compareTo(y._2.rootVowel) else dId
    }
  }

  implicit object StrongVerbContextMarshaller extends Serializer[StrongVerb] {

    override val typeId: Int = 2

    override def marshall(obj: StrongVerb): List[Any] = {

      val vceId = VerbClassEnum idOf obj.verbClass

      val ablautMapLength = obj.ablautGrade.size
      val formsLength = obj.verbForms.size

      val fixOrderedAblautGrade = obj.ablautGrade.toSeq
      val ablautMapAsList =
        fixOrderedAblautGrade.map { case (s, _) => EnumVerbStem idOf s } ++
        fixOrderedAblautGrade.map { case (_, AblautGrade(rv)) => rv }

      val fixOrderedVerbForms = obj.verbForms.toSeq

      val verbFormsAsListKeys: List[Int] = fixOrderedVerbForms.map(_._1).flatMap {

        case (md, ot, op) =>
          val t = ot.map(VerbTenseEnum.idOf(_) + 1).getOrElse(0)
          val p = op.map(Pronoun.idOf(_) + 1).getOrElse(0)
          List(VerbModeEnum idOf md, t, p)

      }.toList

      val verbFormsAsListValues: List[String] = fixOrderedVerbForms.map(_._2).flatMap {

        case StrongVerbForm(repr, StrongVerbStem(stemStr, _, _, _)) => List(repr, stemStr)

      }.toList

      List(vceId, ablautMapLength, formsLength) ++ ablautMapAsList ++ verbFormsAsListKeys ++ verbFormsAsListValues
    }

    override def unmarshall(reader: Reader): StrongVerb = {

      val verbClassEnum: StrongVerbClassEnum = (VerbClassEnum fromId reader[Int](0) get).asInstanceOf[StrongVerbClassEnum]

      val ablautMapSize = reader[Int](1)
      val verbFormsCount = reader[Int](2)

      val ABLAUT_MAP_KEYS_OFFSET = 3
      val ABLAUT_MAP_VALS_OFFSET = ABLAUT_MAP_KEYS_OFFSET + ablautMapSize

      val ablautKeys = (0 until ablautMapSize).map(i => EnumVerbStem fromId reader[Int](ABLAUT_MAP_KEYS_OFFSET + i) get)
      val ablautVals = (0 until ablautMapSize).map(i => reader[String](ABLAUT_MAP_VALS_OFFSET + i))

      val ablautGrades: Map[EnumVerbStem, AblautGrade] = (ablautKeys zip ablautVals)
        .map { case (k, v) => k -> AblautGrade(v) }
        .toMap

      val VERB_FORMS_KEYS_OFFSET = ABLAUT_MAP_KEYS_OFFSET + 2 * ablautMapSize
      val VERB_FORMS_VALS_OFFSET = VERB_FORMS_KEYS_OFFSET + 3 * verbFormsCount
      val verbFormKeys: Seq[VerbType] = (VERB_FORMS_KEYS_OFFSET until VERB_FORMS_VALS_OFFSET by 3)
        .map(i => {

          val mood = VerbModeEnum fromId reader[Int](i) get
          val oti = reader[Int](i + 1)
          val opi = reader[Int](i + 2)

          val ot = if(oti == 0) None else Some(VerbTenseEnum fromId (oti - 1) get)
          val op = if(opi == 0) None else Some(Pronoun fromId (opi - 1) get)

          (mood, ot, op)
        })

      val VERB_FORMS_END_OF_MAP_OFFSET = VERB_FORMS_VALS_OFFSET + 2 * verbFormsCount
      val verbFormRawVals = (VERB_FORMS_VALS_OFFSET until VERB_FORMS_END_OF_MAP_OFFSET by 2)
        .map(i => {
          val verbRepr = reader[String]( i )
          val rootRepr = reader[String](i+1)

          (verbRepr, rootRepr)
        })

      val verbForms: Map[VerbType, StrongVerbForm] = (verbFormKeys zip verbFormRawVals).map {

        case (k, (vR, rR)) => k -> generateStrongVerbFrom(verbClassEnum, ablautGrades, k, vR, rR)

      }.toMap

      StrongVerb(verbClassEnum, ablautGrades, verbForms)
    }

    private def generateStrongVerbFrom(verbClassEnum: StrongVerbClassEnum, ablautGrades: Map[EnumVerbStem, AblautGrade]
      , verbType: VerbType, verbRepr: String, rootRepr: String): StrongVerbForm = verbType match {

      case (k @ (mood: FinitiveMood, Some(tense), Some(pronoun))) =>
        val stemType = tenseAndNumberToStem(tense, pronoun.number)
        val stem = StrongVerbStem(rootRepr, verbClassEnum, stemType)

        FinitiveStrongVerbForm(verbRepr, stem, pronoun, tense, mood)

      case (k @ (mood: NonFinitiveMood, optTense, None)) =>
        val stemType = moodAndTenseToStem(mood, optTense)
        val verbType = toNonFiniteVerbType(optTense, mood)
        val stem = StrongVerbStem(rootRepr, verbClassEnum, stemType)

        NonFinitiveStrongVerbForm(verbRepr, stem, verbType)
    }
  }

  implicit object DictionaryEntryMarshaller extends Serializer[DictionaryEntry] {

    override val typeId: Int = 0

    override def marshall(obj: DictionaryEntry): List[Any] = {

      List(obj.word, obj.meanings.size) ++ obj.meanings
    }

    override def unmarshall(reader: Reader): DictionaryEntry = {

      val word = reader[AnyRef](0)
      val length = reader[Int](1)
      val LIST_OFFSET = 2
      val list = (LIST_OFFSET until LIST_OFFSET + length).map(reader[MeaningDef]).toList
      DictionaryEntry(word, list)
    }
  }

  implicit object MeaningDefMarshaller extends Serializer[MeaningDef] {

    override val typeId: Int = 1

    override def marshall(obj: MeaningDef): List[Any]
      = List(obj.meaning, obj.note, obj.examples.size) ++ obj.examples

    override def unmarshall(reader: Reader): MeaningDef = {

      val meaning = reader[String](0)
      val note = reader[String](1)
      val countOfExamples = reader[Int](2)
      val examples = (3 until 3 + countOfExamples).map(reader[String])

      MeaningDef(meaning, note, examples)
    }
  }
}
