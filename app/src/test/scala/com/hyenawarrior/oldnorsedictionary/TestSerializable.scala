package com.hyenawarrior.oldnorsedictionary

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.INFINITIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum.STRONG_7_1_CLASS
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{INDICATIVE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem.PRESENT_STEM
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import org.junit.Test

import scala.collection.immutable.Map

/**
  * Created by HyenaWarrior on 2018.01.20..
  */
class TestSerializable {

  @Test
  def testDictionaryEntryIsSerializable(): Unit = {

    val md = MeaningDef("", "", Seq(""))
    val frm1 = (VerbModeEnum.INFINITIVE, ACTIVE, None, None)
    val frm2 = (INDICATIVE,  MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_3))
    val frm3 = (SUBJUNCTIVE, ACTIVE,        Some(PAST),    Some(Pronoun.PL_1))
    val stem = StrongVerbStem("", STRONG_7_1_CLASS, PRESENT_STEM)

    val de = DictionaryEntry(StrongVerb(
      STRONG_7_1_CLASS,
      Map(PRESENT_STEM -> AblautGrade("")),
      Map(frm1 -> NonFinitiveStrongVerbForm("", stem, INFINITIVE, ACTIVE)),
      Map(frm2 -> FinitiveStrongVerbForm("", stem, Pronoun.SG_3, PRESENT, INDICATIVE, ACTIVE)),
      Map(frm3 -> FinitiveStrongVerbForm("", stem, Pronoun.PL_1, PAST, SUBJUNCTIVE, ACTIVE))),
      List(md)
    )

    val byteArrayOS = new ByteArrayOutputStream()
    val oss = new ObjectOutputStream(byteArrayOS)

    oss.writeObject(de)
    val byteArray = byteArrayOS.toByteArray

    val iss = new ObjectInputStream(new ByteArrayInputStream(byteArray))
    val obj = iss.readObject()
  }
}
