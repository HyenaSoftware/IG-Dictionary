package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.ACTIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum
import org.junit.Assert._
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.01.20..
  */
class SubjVerbTest {

  @Test
  def testClass2ndChangeToJoForPresent(): Unit = {

    val srcType: VerbType = (SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skyti"))

    assertEquals("skjóta", verb.verbForms(INFINITIVE, ACTIVE, None, None).strRepr)

    assertEquals("skjóta",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("skjótir",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_2)).strRepr)
    assertEquals("skjóti",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strRepr)

    assertEquals("skjótim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)
    assertEquals("skjótið", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_2)).strRepr)
    assertEquals("skjóti",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3)).strRepr)

    assertEquals("skyta",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strRepr)
    assertEquals("skytir",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)
    assertEquals("skyti",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strRepr)

    assertEquals("skytim",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("skytið",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_2)).strRepr)
    assertEquals("skyti",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }

  def testClass5thVerbStressShift(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "sjá"))

    assertEquals("sé",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("sér", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strRepr)
    assertEquals("sém", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)

    assertEquals("sæir", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)
    assertEquals("sæi",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strRepr)

    assertEquals("sæim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("sæi",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }

  @Test
  def testClass5thVerbReka(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("reka",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("reki", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strRepr)
    assertEquals("rekim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)

    assertEquals("rækir", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)
    assertEquals("ræki",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strRepr)

    assertEquals("rækim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("ræki",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }
}
