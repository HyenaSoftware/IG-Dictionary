package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbType, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.ACTIVE
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
}
