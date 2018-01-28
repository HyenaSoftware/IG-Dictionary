package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice._
import org.junit.Assert._
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.01.28..
  */
class SubjMediopassiveVerbTest {

  @Test
  def testClass2ndChange(): Unit = {

    val srcType: VerbType = (INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skauzk"))

    assertEquals("skjótask", verb.verbForms(INFINITIVE, MEDIO_PASSIVE, None, None).strForm)

    // subjunctive
    assertEquals("skjótumk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skjótisk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)

    assertEquals("skjótimsk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("skjótizk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("skjótisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("skytumk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skytisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)

    assertEquals("skytimsk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("skytizk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("skytisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass5thVerbReka(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("rekumk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("rekisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)

    assertEquals("rekimsk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("rekizk",   verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("rekisk",   verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("rækumk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("rækisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("rækisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("rækimsk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("rækizk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("rækisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }
}
