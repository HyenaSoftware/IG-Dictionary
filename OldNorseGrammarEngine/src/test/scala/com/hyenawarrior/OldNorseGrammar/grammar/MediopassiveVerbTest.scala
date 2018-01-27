package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbType}
import org.junit.Assert._
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.01.19..
  */
class MediopassiveVerbTest {

  @Test
  def testClass2ndChange(): Unit = {

    val srcType: VerbType = (INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skaut"))

    assertEquals("skjótask", verb.verbForms(INFINITIVE, MEDIO_PASSIVE, None, None).strForm)

    // indicative
    assertEquals("skjótumk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skýzk",    verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)

    assertEquals("skutumk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skauzk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)

    // subjunctive
    assertEquals("skjótumk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skjótisk", verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)

    assertEquals("skytumk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skytisk",  verb.verbForms(SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
  }
}
