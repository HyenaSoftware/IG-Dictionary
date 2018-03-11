package com.hyenawarrior.oldnorsedictionary

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.StrongStemClassNeuter
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounForm, NounStem}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.NonFinitiveVerbType.INFINITIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum.STRONG_7_1_CLASS
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum.{INDICATIVE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem.PRESENT_STEM
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import org.junit.Test

import scala.collection.immutable.Map

/**
  * Created by HyenaWarrior on 2018.01.20..
  */
class TestSerializable {

  private def trySerialize(obj: AnyRef): Unit = {

    val byteArrayOS = new ByteArrayOutputStream()
    val oss = new ObjectOutputStream(byteArrayOS)

    oss writeObject obj
    val byteArray = byteArrayOS.toByteArray

    val iss = new ObjectInputStream(new ByteArrayInputStream(byteArray))
    iss.readObject()
  }

  @Test
  def testDictionaryEntryIsSerializable(): Unit = {

    val md = MeaningDef("", "", Seq(""))

    val de = DictionaryEntry(StrongVerb(STRONG_7_1_CLASS,
      Map(),
      Map(),
      Map(),
      Map()),
      List(md)
    )

    trySerialize(de)
  }

  @Test
  def testStrongVerbIsSerializable(): Unit = {

    val frm1 = (VerbModeEnum.INFINITIVE, ACTIVE, None, None)
    val frm2 = (INDICATIVE,  MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_3))
    val frm3 = (SUBJUNCTIVE, ACTIVE,        Some(PAST),    Some(Pronoun.PL_1))
    val stem = StrongVerbStem("", STRONG_7_1_CLASS, PRESENT_STEM)

    val verb = StrongVerb(
      STRONG_7_1_CLASS,
      Map(PRESENT_STEM -> AblautGrade("")),
      Map(frm1 -> NonFinitiveStrongVerbForm("", stem, INFINITIVE, ACTIVE)),
      Map(frm2 -> FinitiveStrongVerbForm("", stem, Pronoun.SG_3, PRESENT, INDICATIVE, ACTIVE)),
      Map(frm3 -> FinitiveStrongVerbForm("", stem, Pronoun.PL_1, PAST, SUBJUNCTIVE, ACTIVE)))

    trySerialize(verb)
  }

  @Test
  def testNounIsSerializable(): Unit = {

    val nf = NounForm("", (SINGULAR, NOMINATIVE), isDefinite = false)
    val nft = ((SINGULAR, NOMINATIVE), false)

    val noun = Noun(NounStem("", StrongStemClassNeuter),
      Map(nft -> nf),
      Map(),
      Map())

    trySerialize(noun)
  }
}
