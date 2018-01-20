package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum.STRONG_5TH_CLASS
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{INDICATIVE, INFINITIVE, PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.ACTIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem.{PERFECT_STEM, PRESENT_STEM, PRETERITE_PLURAL_STEM, PRETERITE_SINGULAR_STEM}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.persister.Reader
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2017.11.20..
  */
class TestStrongVerbContextSerializer {

  private def toKeyValuePair(sv: StrongVerbForm): (VerbType, StrongVerbForm) = sv match {

    case fsv: FinitiveStrongVerbForm => (fsv.mood, fsv.voice, Some(fsv.tense), Some(fsv.pronoun)) -> fsv
    case nsv: NonFinitiveStrongVerbForm =>
      val optTense = nsv.nonFinitiveVerbType match {
        case NonFinitiveVerbType(_, PRESENT_STEM, INFINITIVE) => None
        case NonFinitiveVerbType(_, PRESENT_STEM, PARTICIPLE) => Some(PRESENT)
        case NonFinitiveVerbType(_, PERFECT_STEM, PARTICIPLE) => Some(VerbTenseEnum.PAST)
      }
      (nsv.nonFinitiveVerbType.mood, nsv.voice, optTense, None) -> nsv
  }

  case class MyReader(data: List[Any]) extends Reader {

    private val indexedData = data.toIndexedSeq

    override def apply[T](i: Int)(implicit clazz: ClassTag[T]): T = indexedData(i).asInstanceOf[T]
  }

  @Test
  def testSerializeAndBack(): Unit = {

    val serializer = serializers.StrongVerbContextMarshaller

    val ABLAUT_GRADES = Map(
      PRESENT_STEM -> AblautGrade("e"),
      PRETERITE_SINGULAR_STEM -> AblautGrade("a"),
      PRETERITE_PLURAL_STEM -> AblautGrade("á"),
      PERFECT_STEM -> AblautGrade("e")
    )

    val VERB_FORMS: Map[VerbType, StrongVerbForm] = Map(

      toKeyValuePair(NonFinitiveStrongVerbForm("liggja",StrongVerbStem("leg", STRONG_5TH_CLASS, PRESENT_STEM), NonFinitiveVerbType.INFINITIVE, ACTIVE)),
      toKeyValuePair(NonFinitiveStrongVerbForm("leginn",StrongVerbStem("leg", STRONG_5TH_CLASS, PERFECT_STEM), NonFinitiveVerbType.PAST_PARTICIPLE, ACTIVE)),
      toKeyValuePair(FinitiveStrongVerbForm("ligg", StrongVerbStem("le", STRONG_5TH_CLASS, PRESENT_STEM), Pronoun.SG_1, PRESENT, INDICATIVE, ACTIVE)),
      toKeyValuePair(FinitiveStrongVerbForm("liggr",StrongVerbStem("le", STRONG_5TH_CLASS, PRESENT_STEM), Pronoun.SG_3, PRESENT, INDICATIVE, ACTIVE)),
      toKeyValuePair(FinitiveStrongVerbForm("lá",   StrongVerbStem("la", STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM),Pronoun.SG_3, PAST, INDICATIVE, ACTIVE)),
      toKeyValuePair(FinitiveStrongVerbForm("lágum",StrongVerbStem("lág",STRONG_5TH_CLASS, PRETERITE_PLURAL_STEM),  Pronoun.PL_1, PAST, INDICATIVE, ACTIVE))
    )

    val verb = StrongVerb(STRONG_5TH_CLASS, ABLAUT_GRADES, VERB_FORMS)
    val data = serializer.marshall(verb)

    val sameVerb = serializer.unmarshall(MyReader(data))

    assertEquals(verb.verbClass, sameVerb.verbClass)
    assertEquals(verb.ablautGrade, sameVerb.ablautGrade)
    assertEquals(verb.verbForms.toSeq.map(_._2.getStem), sameVerb.verbForms.toSeq.map(_._2.getStem))
  }

  @Test
  def testDictionaryEntrySerializer(): Unit = {

    val serializer = serializers.DictionaryEntryMarshaller

    val meanings = List(MeaningDef("m1", "n1", Seq("e1", "e2")))
    val de = DictionaryEntry(StrongVerb(STRONG_5TH_CLASS, Map(), Map()), meanings)

    val data = serializer.marshall(de)

    val sameDe = serializer.unmarshall(MyReader(data))

    assertEquals(de.word, sameDe.word)
    assertEquals(de.meanings, sameDe.meanings)
  }
}
