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

    assertEquals("skjótask", verb.verbForms(INFINITIVE, MEDIO_PASSIVE, None, None).strRepr)

    assertEquals("skjótandisk", verb.verbForms(PARTICIPLE, MEDIO_PASSIVE, Some(PRESENT), None).strRepr)
    assertEquals("skotizk",     verb.verbForms(PARTICIPLE, MEDIO_PASSIVE, Some(PAST),    None).strRepr)

    // indicative
    assertEquals("skjótumk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("skýzk",    verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_2)).strRepr)

    assertEquals("skjótumsk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)
    assertEquals("skjótizk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strRepr)
    assertEquals("skjótask",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strRepr)

    assertEquals("skutumk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strRepr)
    assertEquals("skauzk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)

    assertEquals("skutumsk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("skutuzk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strRepr)
    assertEquals("skutusk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }

  @Test
  def testClass5thVerbReka(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("rekumk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_1)).strRepr)
    assertEquals("reksk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.SG_3)).strRepr)

    assertEquals("rekumsk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_1)).strRepr)
    assertEquals("rekizk",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_2)).strRepr)
    assertEquals("rekask",   verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun.PL_3)).strRepr)

    assertEquals("rákumk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_1)).strRepr)
    assertEquals("rakzk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)
    assertEquals("raksk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_3)).strRepr)

    assertEquals("rákumsk", verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_1)).strRepr)
    assertEquals("rákuzk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_2)).strRepr)
    assertEquals("rákusk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.PL_3)).strRepr)
  }

  @Test
  def testClass5thVerbLiggja(): Unit ={

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "liggja"))

    assertEquals("lázk",  verb.verbForms(INDICATIVE, MEDIO_PASSIVE, Some(PAST), Some(Pronoun.SG_2)).strRepr)
  }
}
