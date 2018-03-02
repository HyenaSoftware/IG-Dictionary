package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
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

    assertEquals("skjótask", verb.verbForms(INFINITIVE, MEDIO_PASSIVE, None, None).strRepr)

    // subjunctive
    assertEquals("skjótumk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("skjótisk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_2)).strRepr)

    assertEquals("skjótimsk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)
    assertEquals("skjótizk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strRepr)
    assertEquals("skjótisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strRepr)

    assertEquals("skytumk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strRepr)
    assertEquals("skytisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)

    assertEquals("skytimsk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("skytizk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strRepr)
    assertEquals("skytisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }

  @Test
  def testClass5thVerbReka(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("rekumk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("rekisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_3)).strRepr)

    assertEquals("rekimsk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)
    assertEquals("rekizk",   verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strRepr)
    assertEquals("rekisk",   verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strRepr)

    assertEquals("rækumk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strRepr)
    assertEquals("rækisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)
    assertEquals("rækisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_3)).strRepr)

    assertEquals("rækimsk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("rækizk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strRepr)
    assertEquals("rækisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }
}
