package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounForm, NounStem}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.FinitiveStrongVerbForm.tenseAndNumberToStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveStrongVerbForm.{moodAndTenseToStem, toNonFiniteVerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}
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
    classOf[Noun] -> NounMarshaller,
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

  // 2 -strong verbs
  implicit object StrongVerbContextMarshaller extends Serializer[StrongVerb] {

    override val typeId: Int = 2

    override def marshall(obj: StrongVerb): List[Any] = {

      val vceId: Byte = (VerbClassEnum idOf obj.verbClass).toByte

      //
      val ablautData = serializeMap(obj.ablautGrade) {

        case e: EnumVerbStem => List((EnumVerbStem idOf e).toByte)
        case AblautGrade(rv) => List(rv)
      }

      //
      val givenMapData: List[Any] = serializeMap(obj.givenVerbForms)(serializeMapElem)
      val generatedMapData: List[Any] = serializeMap(obj.generatedVerbForms)(serializeMapElem)
      val overriddenMapData: List[Any] = serializeMap(obj.overriddenVerbForms)(serializeMapElem)

      vceId +: (ablautData ++ givenMapData ++ generatedMapData ++ overriddenMapData)
    }

    private def serializeMapElem(a: Any): List[Any] = a match {

      case (md: VerbModeEnum, voice: VerbVoice, ot: Option[VerbTenseEnum], op: Option[Pronoun]) =>
        val v = VerbVoice idOf voice
        val t = ot.map(VerbTenseEnum.idOf(_) + 1).getOrElse(0)
        val p = op.map(Pronoun.idOf(_) + 1).getOrElse(0)
        List((VerbModeEnum idOf md) toByte, v toByte, t toByte, p toByte)

      case StrongVerbForm(repr, StrongVerbStem(stemStr, _, _, _)) => List(repr, stemStr)
    }

    private def serializeMap[K, V](m: Map[K, V])(f: Any => List[Any]): List[Any] = {

      val fixOrderedMap = m.toSeq
      val keys = fixOrderedMap.flatMap { case (k, _) => f(k) }
      val vals = fixOrderedMap.flatMap { case (_, v) => f(v) }

      m.size.toByte +: (keys ++ vals).toList
    }

    override def unmarshall(reader: Reader): StrongVerb = {

      val vceId = reader[Byte]()
      val verbClassEnum = (VerbClassEnum fromId vceId get).asInstanceOf[StrongVerbClassEnum]

      //
      val ablautGrades = deserializeMap(reader, EnumVerbStem fromId reader[Byte]() get, AblautGrade(reader[String]()))

      //
      val givenVerbForms = generateMapEntry(reader, verbClassEnum, ablautGrades)
      val generatedVerbForms = generateMapEntry(reader, verbClassEnum, ablautGrades)
      val overriddenVerbForms = generateMapEntry(reader, verbClassEnum, ablautGrades)

      StrongVerb(verbClassEnum, ablautGrades, givenVerbForms, generatedVerbForms, overriddenVerbForms)
    }

    private def deserializeMap[K, V](reader: Reader, fk: => K, fv: => V): Map[K, V] = {

      val mapLength = reader[Byte]()

      val keys = (0 until mapLength).map(_ => fk)
      val vals = (0 until mapLength).map(_ => fv)

      (keys zip vals).toMap
    }

    private def deserializeKey(reader: Reader): VerbType = {

      val mood = VerbModeEnum fromId reader[Byte]() get
      val voice = VerbVoice fromId reader[Byte]() get
      val oti = reader[Byte]()
      val opi = reader[Byte]()

      val ot = if(oti == 0) None else Some(VerbTenseEnum fromId (oti - 1) get)
      val op = if(opi == 0) None else Some(Pronoun fromId (opi - 1) get)

      (mood, voice, ot, op)
    }

    private def deserializeValue(reader: Reader): (String, String) = {

      val verbRepr = reader[String]()
      val rootRepr = reader[String]()

      (verbRepr, rootRepr)
    }

    private def generateMapEntry(reader: Reader, verbClassEnum: StrongVerbClassEnum
      , ablautGrades: Map[EnumVerbStem, AblautGrade]): Map[VerbType, StrongVerbForm] = {

      val verbMap = deserializeMap(reader, deserializeKey(reader), deserializeValue(reader))

      verbMap.map {

        case (vt, (vR, rR)) => vt -> generateStrongVerbFrom(verbClassEnum, ablautGrades, vt, vR, rR)
      }
    }

    private def generateStrongVerbFrom(verbClassEnum: StrongVerbClassEnum, ablautGrades: Map[EnumVerbStem, AblautGrade]
      , verbType: VerbType, verbRepr: String, rootRepr: String): StrongVerbForm = verbType match {

      case (mood: FinitiveMood, voice, Some(tense), Some(pronoun)) =>
        val stemType = tenseAndNumberToStem(tense, pronoun.number)
        val stem = StrongVerbStem(rootRepr, verbClassEnum, stemType)

        FinitiveStrongVerbForm(verbRepr, stem, pronoun, tense, mood, voice)

      case (mood: NonFinitiveMood, voice, optTense, None) =>
        val stemType = moodAndTenseToStem(mood, optTense)
        val verbType = toNonFiniteVerbType(optTense, mood)
        val stem = StrongVerbStem(rootRepr, verbClassEnum, stemType)

        NonFinitiveStrongVerbForm(verbRepr, stem, verbType, voice)
    }
  }

  // 3 - nouns
  implicit object NounMarshaller extends Serializer[Noun] {

    override val typeId: Int = 3

    override def marshall(obj: Noun): List[Any] = {

      val rootRepr = obj.stem.rootStr

      val nsce = NounStemClassEnum.values.find(e => e.nounStemClass == obj.stem.stemClass).get
      val nsceId = (NounStemClassEnum idOf nsce).toByte

      val givenFormsStream     = obj.givenForms.size.toByte     +: obj.givenForms.values.flatMap(serializeNounForm).toList
      val generatedFormsStream = obj.generatedForms.size.toByte +: obj.generatedForms.values.flatMap(serializeNounForm).toList
      val overridenFormsStream = obj.overridenForms.size.toByte +: obj.overridenForms.values.flatMap(serializeNounForm).toList

      List(rootRepr, nsceId.toByte) ++ givenFormsStream ++ generatedFormsStream ++ overridenFormsStream
    }

    private def serializeNounForm(obj: NounForm): List[Any] = {

      val (number, caze) = obj.declension
      val numberId = (GNumber idOf number).toByte
      val caseId   = (Case idOf caze).toByte

      List(obj.strRepr, numberId, caseId)
    }

    override def unmarshall(reader: Reader): Noun = {

      val rootRepr = reader[String]()
      val stemClassId = reader[Byte]()

      val stemClass = NounStemClassEnum.fromId(stemClassId).map(_.nounStemClass).get
      val stem = NounStem(rootRepr, stemClass)

      val givenForms     = deserializeList(reader).map(e => e.declension -> e).toMap
      val generatedForms = deserializeList(reader).map(e => e.declension -> e).toMap
      val overridenForms = deserializeList(reader).map(e => e.declension -> e).toMap

      Noun(stem, givenForms, generatedForms, overridenForms)
    }

    private def deserializeList(reader: Reader): List[NounForm] = {

      val size = reader[Byte]()

      (0 until size).map(i => createMapValue(reader)).toList
    }

    private def createMapValue(reader: Reader): NounForm = {

      val strRepr = reader[String]()
      val numberId = reader[Byte]()
      val caseId   = reader[Byte]()

      val number = GNumber fromId numberId get
      val caze   = Case fromId caseId get

      val declension = number -> caze

      NounForm(strRepr, declension)
    }
  }

  // 0 - dictionary entry
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

  // 1 - meaning definition
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
