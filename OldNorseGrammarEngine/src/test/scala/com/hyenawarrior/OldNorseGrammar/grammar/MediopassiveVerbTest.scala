package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{apply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbType}
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
  }

  @Test
  def testClass5thVerbReka(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("rekumk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("reksk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)

    assertEquals("rekumsk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("rekizk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("rekask",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("rákumk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("rakzk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("raksk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("rákumsk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("rákuzk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("rákusk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass5thVerbLiggja(): Unit ={

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "liggja"))

    assertEquals("lázk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
  }
}
