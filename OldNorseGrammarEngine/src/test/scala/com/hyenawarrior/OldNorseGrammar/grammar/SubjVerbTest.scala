package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import org.junit.Assert._
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.01.20..
  */
class SubjVerbTest {

  @Test
  def testClass2ndChangeToJoForPresent(): Unit = {

    val srcType: VerbType = (SUBJUNCTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skyti"))

    assertEquals("skjóta", verb.verbForms(INFINITIVE, None, None).strForm)

    assertEquals("skjóta",   verb.verbForms(SUBJUNCTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skjótir",  verb.verbForms(SUBJUNCTIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)
    assertEquals("skjóti",   verb.verbForms(SUBJUNCTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)

    assertEquals("skjótim", verb.verbForms(SUBJUNCTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("skjótið", verb.verbForms(SUBJUNCTIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("skjóti",  verb.verbForms(SUBJUNCTIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("skyta",   verb.verbForms(SUBJUNCTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skytir",  verb.verbForms(SUBJUNCTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("skyti",   verb.verbForms(SUBJUNCTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("skytim",  verb.verbForms(SUBJUNCTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("skytið",  verb.verbForms(SUBJUNCTIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("skyti",   verb.verbForms(SUBJUNCTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }
}
