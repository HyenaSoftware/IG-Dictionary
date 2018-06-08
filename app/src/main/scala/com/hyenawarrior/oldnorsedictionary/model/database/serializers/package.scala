package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounForm, NounStem}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.FinitiveStrongVerbForm.tenseAndNumberToStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveStrongVerbForm.{moodAndTenseToStem, toNonFiniteVerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{FinitiveMood, NonFinitiveMood, StrongVerbClassEnum, VerbClassEnum, VerbModeEnum, VerbTenseEnum, VerbVoice, WeakVerbClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, WeakVerbStem}
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.persister.{Reader, Serializer}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import com.hyenawarrior.oldnorsedictionary.modelview.helpers._

import scala.language.postfixOps

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
package object serializers {

  implicit object AblautGradeOrdering extends Ordering[(EnumVerbStem, AblautGrade)] {

    override def compare(x: (EnumVerbStem, AblautGrade), y: (EnumVerbStem, AblautGrade)): Int = {

      val xId = EnumVerbStem.idOf(x._1)
      val yId = EnumVerbStem.idOf(y._1)
      val dId = yId - xId
      if(dId == 0) x._2.rootVowel.compareTo(y._2.rootVowel) else dId
    }
  }

  trait VerbSerializer[V] extends Serializer[V] {

    def serializeMapElem(a: Any): List[Any] = a match {

      case (md: VerbModeEnum, voice: VerbVoice, ot: Option[VerbTenseEnum], op: Option[Pronoun]) =>
        val v = VerbVoice idOf voice
        val t = ot.map(VerbTenseEnum.idOf(_) + 1).getOrElse(0)
        val p = op.map(Pronoun.idOf(_) + 1).getOrElse(0)
        List((VerbModeEnum idOf md) toByte, v toByte, t toByte, p toByte)

      case StrongVerbForm(repr, StrongVerbStem(stemStr, _, _, _)) => List(repr, stemStr)
      case WeakVerbForm(repr, WeakVerbStem(stemStr, _, _, _)) => List(repr, stemStr)
    }

    def serializeMap[K, V](m: Map[K, V])(f: Any => List[Any]): List[Any] = {

      val fixOrderedMap = m.toSeq
      val keys = fixOrderedMap.flatMap { case (k, _) => f(k) }
      val vals = fixOrderedMap.flatMap { case (_, v) => f(v) }

      m.size.toByte +: (keys ++ vals).toList
    }

    def deserializeMap[K, V](reader: Reader, fk: => K, fv: => V): Map[K, V] = {

      val mapLength = reader[Byte]()

      val keys = (0 until mapLength).map(_ => fk)
      val vals = (0 until mapLength).map(_ => fv)

      (keys zip vals).toMap
    }

    def deserializeKey(reader: Reader): VerbType = {

      val mood = VerbModeEnum findById reader[Byte]() get
      val voice = VerbVoice findById reader[Byte]() get
      val oti = reader[Byte]()
      val opi = reader[Byte]()

      val ot = if(oti == 0) None else Some(VerbTenseEnum findById (oti - 1) get)
      val op = if(opi == 0) None else Some(Pronoun findById (opi - 1) get)

      (mood, voice, ot, op)
    }

    def deserializeValue(reader: Reader): (String, String) = {

      val verbRepr = reader[String]()
      val rootRepr = reader[String]()

      (verbRepr, rootRepr)
    }

    def generateMapEntry[VF <: VerbForm](reader: Reader, generateVerbFrom: (VerbType, String, String) => VF): Map[VerbType, VF] = {

      val verbMap = deserializeMap(reader, deserializeKey(reader), deserializeValue(reader))

      verbMap.map {

        case (vt, (vR, rR)) => vt -> generateVerbFrom(vt, vR, rR)
      }
    }
  }

  // 2 -strong verbs
  implicit object StrongVerbContextMarshaller extends VerbSerializer[StrongVerb] {

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

    override def unmarshall(reader: Reader): StrongVerb = {

      val vceId = reader[Byte]()
      val verbClassEnum = (VerbClassEnum findById vceId get).asInstanceOf[StrongVerbClassEnum]

      //
      def verbStemEnum = EnumVerbStem findById reader[Byte]() get
      val ablautGrades = deserializeMap(reader, verbStemEnum, AblautGrade(reader[String]()))

      //
      val vfGen = generateVerbFrom(verbClassEnum, ablautGrades)(_, _, _)

      val givenVerbForms = generateMapEntry(reader, vfGen)
      val generatedVerbForms = generateMapEntry(reader, vfGen)
      val overriddenVerbForms = generateMapEntry(reader, vfGen)

      new StrongVerb(verbClassEnum, ablautGrades, givenVerbForms, generatedVerbForms, overriddenVerbForms)
    }

    private def generateVerbFrom(verbClassEnum: StrongVerbClassEnum, ablautGrades: Map[EnumVerbStem, AblautGrade])
                                         (vt: VerbType, verbRepr: String, rootRepr: String): StrongVerbForm = vt match {

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

  // 4 - weak verbs
  implicit object WeakVerbMarshaller extends VerbSerializer[WeakVerb] {

    override val typeId: Int = 4

    override def marshall(obj: WeakVerb): List[Any] = {

      val vceId: Byte = (VerbClassEnum idOf obj.verbClass).toByte

      //
      val givenMapData: List[Any] = serializeMap(obj.givenVerbForms)(serializeMapElem)
      val generatedMapData: List[Any] = serializeMap(obj.generatedVerbForms)(serializeMapElem)
      val overriddenMapData: List[Any] = serializeMap(obj.overriddenVerbForms)(serializeMapElem)

      vceId +: (givenMapData ++ generatedMapData ++ overriddenMapData)
    }

    override def unmarshall(reader: Reader): WeakVerb = {

      val vceId = reader[Byte]()
      val verbClassEnum = (VerbClassEnum findById vceId get).asInstanceOf[WeakVerbClassEnum]

      //
      val vfGen = generateVerbFrom(verbClassEnum)(_, _, _)

      //
      val givenVerbForms = generateMapEntry(reader, vfGen)
      val generatedVerbForms = generateMapEntry(reader, vfGen)
      val overriddenVerbForms = generateMapEntry(reader, vfGen)

      new WeakVerb(verbClassEnum, givenVerbForms, generatedVerbForms, overriddenVerbForms)
    }

    private def generateVerbFrom(verbClassEnum: WeakVerbClassEnum)
                                (vt: VerbType, verbRepr: String, rootRepr: String): WeakVerbForm = {
      val (stemType, voice) = vt match {

        case (mood: FinitiveMood, v, Some(tense), Some(pronoun)) =>
          (tenseAndNumberToStem(tense, pronoun.number), v)

        case (mood: NonFinitiveMood, v, optTense, None) =>
          (moodAndTenseToStem(mood, optTense), v)
      }

      val stem = WeakVerbStem(rootRepr, verbClassEnum, stemType)

      new WeakVerbForm(verbRepr, stem, voice)
    }
  }

  // 3 - nouns
  implicit object NounMarshaller extends Serializer[Noun] {

    override val typeId: Int = 3

    override def marshall(obj: Noun): List[Any] = {

      val rootRepr = obj.stem.rootStr

      val nsce = obj.stem.stemClass.asInstanceOf[NounStemClassEnum]
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

      List(obj.strRepr, numberId, caseId, obj.isDefinite)
    }

    override def unmarshall(reader: Reader): Noun = {

      val rootRepr = reader[String]()
      val stemClassId = reader[Byte]()

      val stemClass = NounStemClassEnum.findById(stemClassId).get
      val stem = NounStem(rootRepr, stemClass)

      val givenForms     = deserializeList(reader).map(e => e.declension -> e.isDefinite -> e).toMap
      val generatedForms = deserializeList(reader).map(e => e.declension -> e.isDefinite -> e).toMap
      val overridenForms = deserializeList(reader).map(e => e.declension -> e.isDefinite -> e).toMap

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
      val isDefinite = reader[Boolean]()

      val number = GNumber findById numberId get
      val caze   = Case findById caseId get

      val declension = number -> caze

      NounForm(strRepr, declension, isDefinite)
    }
  }

  // 0 - dictionary entry
  implicit object DictionaryEntryMarshaller extends Serializer[DictionaryEntry] {

    override val typeId: Int = 0

    override def marshall(obj: DictionaryEntry): List[Any] = {

      val wordData = obj.word match {

        case sv: StrongVerb => StrongVerbContextMarshaller.typeId.toByte +: (StrongVerbContextMarshaller marshall sv)
        case wv: WeakVerb => WeakVerbMarshaller.typeId.toByte +: (WeakVerbMarshaller marshall wv)
        case nn: Noun => NounMarshaller.typeId.toByte +: (NounMarshaller marshall nn)
      }

      val meaningData: List[Any] = obj.meanings.flatMap(MeaningDefMarshaller.marshall)

      List(1.toByte) ++ wordData ++ List(obj.meanings.size.toByte) ++ meaningData
    }

    override def unmarshall(reader: Reader): DictionaryEntry = {

      // it's just a placeholder currently it is not used
      val altFormCount = reader[Byte]()

      val objType = reader[Byte]()

      val word = objType match {
        case StrongVerbContextMarshaller.typeId => StrongVerbContextMarshaller.unmarshall(reader)
        case WeakVerbMarshaller.typeId => WeakVerbMarshaller.unmarshall(reader)
        case NounMarshaller.typeId => NounMarshaller.unmarshall(reader)
      }

      val length = reader[Byte]()
      val list = (0 until length).toList.map(i => MeaningDefMarshaller.unmarshall(reader))
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

  object HashCode {

    def byteArrayFrom(obj: StrongVerb): Array[Byte] = {

      val gf = obj.givenVerbForms.toSeq.sortBy(_._1).map(_._2.strRepr).mkString
      val of = obj.overriddenVerbForms.toSeq.sortBy(_._1).map(_._2.strRepr).mkString

      obj.verbClass.id().toByte +: Array[Byte]() ++: gf.getBytes ++: of.getBytes
    }

    def byteArrayFrom(obj: WeakVerb): Array[Byte] = {

      val gf = obj.givenVerbForms.toSeq.sortBy(_._1).map(_._2.strRepr).mkString
      val of = obj.overriddenVerbForms.toSeq.sortBy(_._1).map(_._2.strRepr).mkString

      obj.verbClass.id().toByte +: gf.getBytes ++: of.getBytes
    }

    def byteArrayFrom(obj: Noun): Array[Byte] = {

      val gf = obj.givenForms.toSeq.sortBy(_._1).map(_._2.strRepr).mkString
      val of = obj.overridenForms.toSeq.sortBy(_._1).map(_._2.strRepr).mkString

      gf.getBytes ++ of.getBytes
    }

    def byteArrayFrom(obj: List[MeaningDef]): Array[Byte] = {

      val str = obj.map(e => e.meaning + e.note + e.examples.mkString).mkString

      str.getBytes
    }

    def fnv1(ba: Array[Byte]): Int = {

      // FNV-1
      //                 2166136261
      val offset_basis = 0x811C9DC5

      val fnv_prime = 16777619

      var hash = offset_basis
      for(b <- ba) {

        hash = hash ^ b
        hash = hash * fnv_prime
      }

      hash
    }
  }

  trait HashCode[T] {

    def apply(obj: T): Int
  }

  implicit object DictionaryEntryHashCode extends HashCode[DictionaryEntry] {

    override def apply(obj: DictionaryEntry): Int = {

      val word = obj.word match {

        case sv: StrongVerb => HashCode.byteArrayFrom(sv)
        case wv: WeakVerb => HashCode.byteArrayFrom(wv)
        case nn: Noun => HashCode.byteArrayFrom(nn)
      }

      HashCode.fnv1(word ++ HashCode.byteArrayFrom(obj.meanings))
    }
  }
}
