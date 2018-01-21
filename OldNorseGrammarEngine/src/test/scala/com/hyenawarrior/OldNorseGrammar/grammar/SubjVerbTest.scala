package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.ACTIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbType}
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

    assertEquals("skjóta", verb.verbForms(INFINITIVE, ACTIVE, None, None).strForm)

    assertEquals("skjóta",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skjótir",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)
    assertEquals("skjóti",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)

    assertEquals("skjótim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("skjótið", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("skjóti",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("skyta",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skytir",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("skyti",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("skytim",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("skytið",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("skyti",   verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  def testClass5thVerbStressShift(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "sjá"))

    assertEquals("sé",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("sér", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("sém", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)

    assertEquals("sæir", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("sæi",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("sæim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("sæi",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass5thVerb(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("reka",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("reki", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("rekim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)

    assertEquals("rækir", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("ræki",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("rækim", verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("ræki",  verb.verbForms(SUBJUNCTIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }
}
