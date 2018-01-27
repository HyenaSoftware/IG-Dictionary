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

    private val VERB_FORM_KEY_SIZE = 4

    override def marshall(obj: StrongVerb): List[Any] = {

      val vceId: Byte = (VerbClassEnum idOf obj.verbClass).toByte

      val ablautMapLength: Byte = obj.ablautGrade.size.toByte
      val formsLength: Byte = obj.verbForms.size.toByte

      val fixOrderedAblautGrade = obj.ablautGrade.toSeq
      val ablautMapAsListKeys = fixOrderedAblautGrade.map { case (s, _) => (EnumVerbStem idOf s).toByte }
      val ablautMapAsListVals = fixOrderedAblautGrade.map { case (_, AblautGrade(rv)) => rv }

      val fixOrderedVerbForms = obj.verbForms.toSeq

      val verbFormsAsListKeys: List[Byte] = fixOrderedVerbForms.map(_._1).flatMap {

        case (md, voice, ot, op) =>
          val v = VerbVoice idOf voice
          val t = ot.map(VerbTenseEnum.idOf(_) + 1).getOrElse(0)
          val p = op.map(Pronoun.idOf(_) + 1).getOrElse(0)
          List((VerbModeEnum idOf md) toByte, v toByte, t toByte, p toByte)
      }.toList

      val verbFormsAsListValues: List[String] = fixOrderedVerbForms.map(_._2).flatMap {

        case StrongVerbForm(repr, StrongVerbStem(stemStr, _, _, _)) => List(repr, stemStr)

      }.toList

      List(vceId, ablautMapLength, formsLength) ++
        ablautMapAsListKeys ++ ablautMapAsListVals ++
        verbFormsAsListKeys ++ verbFormsAsListValues
    }

    override def unmarshall(reader: Reader): StrongVerb = {

      val vceId = reader[Byte]()
      val verbClassEnum: StrongVerbClassEnum = (VerbClassEnum fromId vceId get).asInstanceOf[StrongVerbClassEnum]

      val ablautMapSize = reader[Byte]()
      val verbFormsCount = reader[Byte]()

      val ABLAUT_MAP_KEYS_OFFSET = 3
      val ABLAUT_MAP_VALS_OFFSET = ABLAUT_MAP_KEYS_OFFSET + ablautMapSize

      val ablautKeys = (0 until ablautMapSize).map(i => EnumVerbStem fromId reader[Byte]() get)
      val ablautVals = (0 until ablautMapSize).map(i => reader[String]())

      val ablautGrades: Map[EnumVerbStem, AblautGrade] = (ablautKeys zip ablautVals)
        .map { case (k, v) => k -> AblautGrade(v) }
        .toMap

      val VERB_FORMS_KEYS_OFFSET = ABLAUT_MAP_KEYS_OFFSET + 2 * ablautMapSize
      val VERB_FORMS_VALS_OFFSET = VERB_FORMS_KEYS_OFFSET + VERB_FORM_KEY_SIZE * verbFormsCount
      val verbFormKeys: Seq[VerbType] = (VERB_FORMS_KEYS_OFFSET until VERB_FORMS_VALS_OFFSET by VERB_FORM_KEY_SIZE)
        .map(i => {

          val mood = VerbModeEnum fromId reader[Byte]() get
          val voice = VerbVoice fromId reader[Byte]() get
          val oti = reader[Byte]()
          val opi = reader[Byte]()

          val ot = if(oti == 0) None else Some(VerbTenseEnum fromId (oti - 1) get)
          val op = if(opi == 0) None else Some(Pronoun fromId (opi - 1) get)

          (mood, voice, ot, op)
        })

      val VERB_FORMS_END_OF_MAP_OFFSET = VERB_FORMS_VALS_OFFSET + 2 * verbFormsCount
      val verbFormRawVals = (VERB_FORMS_VALS_OFFSET until VERB_FORMS_END_OF_MAP_OFFSET by 2)
        .map(i => {
          val verbRepr = reader[String]()
          val rootRepr = reader[String]()

          (verbRepr, rootRepr)
        })

      val verbForms: Map[VerbType, StrongVerbForm] = (verbFormKeys zip verbFormRawVals).map {

        case (vt, (vR, rR)) => vt -> generateStrongVerbFrom(verbClassEnum, ablautGrades, vt, vR, rR)

      }.toMap

      StrongVerb(verbClassEnum, ablautGrades, verbForms)
    }

    private def generateStrongVerbFrom(verbClassEnum: StrongVerbClassEnum, ablautGrades: Map[EnumVerbStem, AblautGrade]
      , verbType: VerbType, verbRepr: String, rootRepr: String): StrongVerbForm = verbType match {

      case (k @ (mood: FinitiveMood, voice, Some(tense), Some(pronoun))) =>
        val stemType = tenseAndNumberToStem(tense, pronoun.number)
        val stem = StrongVerbStem(rootRepr, verbClassEnum, stemType)

        FinitiveStrongVerbForm(verbRepr, stem, pronoun, tense, mood, voice)

      case (k @ (mood: NonFinitiveMood, voice, optTense, None)) =>
        val stemType = moodAndTenseToStem(mood, optTense)
        val verbType = toNonFiniteVerbType(optTense, mood)
        val stem = StrongVerbStem(rootRepr, verbClassEnum, stemType)

        NonFinitiveStrongVerbForm(verbRepr, stem, verbType, voice)
    }
  }

  implicit object DictionaryEntryMarshaller extends Serializer[DictionaryEntry] {

    override val typeId: Int = 0

    override def marshall(obj: DictionaryEntry): List[Any] = {

      List(obj.word, obj.meanings.size.toByte) ++ obj.meanings
    }

    override def unmarshall(reader: Reader): DictionaryEntry = {

      val word = reader[AnyRef]()
      val length = reader[Byte]()
      val list = (0 until length).map(i => reader[MeaningDef]()).toList
      DictionaryEntry(word, list)
    }
  }

  implicit object MeaningDefMarshaller extends Serializer[MeaningDef] {

    override val typeId: Int = 1

    override def marshall(obj: MeaningDef): List[Any]
      = List(obj.meaning, obj.note, obj.examples.size.toByte) ++ obj.examples

    override def unmarshall(reader: Reader): MeaningDef = {

      val meaning = reader[String]()
      val note = reader[String]()
      val countOfExamples = reader[Byte]()
      val examples = (0 until countOfExamples).map(i => reader[String]())

      MeaningDef(meaning, note, examples)
    }
  }
}
