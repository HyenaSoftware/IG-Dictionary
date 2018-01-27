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

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skauzk"))

    assertEquals("skjótask", verb.verbForms(INFINITIVE, MEDIO_PASSIVE, None, None).strForm)

    assertEquals("skjótandisk", verb.verbForms(PARTICIPLE, MEDIO_PASSIVE, Some(PRESENT), None).strForm)
    assertEquals("skotizk",     verb.verbForms(PARTICIPLE, MEDIO_PASSIVE, Some(PAST),    None).strForm)

    // indicative
    assertEquals("skjótumk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skýzk",    verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)

    assertEquals("skjótumsk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("skjótizk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("skjótask",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("skutumk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skauzk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)

    assertEquals("skutumsk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("skutuzk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("skutusk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)

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
}
