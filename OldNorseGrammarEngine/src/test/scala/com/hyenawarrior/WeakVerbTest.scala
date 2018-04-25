package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.VerbTestAux.diff
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.04.15..
  */
class WeakVerbTest {

  @Test
  def testAStemKalla(): Unit = diff(WEAK_A_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "kalla",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "kallað",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "kallað",
      EnumVerbStem.PERFECT_STEM -> "kallað"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "kalla",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "kallaðr",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "kallar",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "kǫllum",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "kallið",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "kalla",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "kallaða",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "kallaði",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "kǫlluðum",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "kǫlluðu",

      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "kalli",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "kallim",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "kallið",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "kalli",

      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "kallaða",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "kallaði",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "kallaðim",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "kallaði",

      (INFINITIVE, MEDIO_PASSIVE, None,          None)       -> "kallask",
      (PARTICIPLE, MEDIO_PASSIVE, Some(PAST),    None)       -> "kallazk",

      (INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(SG_1)) -> "kǫllumk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(PL_1)) -> "kǫllumsk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(PL_2)) -> "kallizk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(PL_3)) -> "kallask",

      (INDICATIVE, MEDIO_PASSIVE, Some(PAST),    Some(SG_1)) -> "kǫlluðumk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PAST),    Some(SG_3)) -> "kallaðisk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PAST),    Some(PL_1)) -> "kǫlluðumsk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PAST),    Some(PL_2)) -> "kǫlluðuzk",
      (INDICATIVE, MEDIO_PASSIVE, Some(PAST),    Some(PL_3)) -> "kǫlluðusk",

      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(SG_1)) -> "kǫllumk",
      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(PL_1)) -> "kallimsk",
      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(PL_2)) -> "kallizk",
      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PRESENT), Some(PL_3)) -> "kallisk",

      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST),    Some(SG_1)) -> "kǫlluðumk",
      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST),    Some(SG_3)) -> "kallaðisk",
      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST),    Some(PL_1)) -> "kallaðimsk",
      (SUBJUNCTIVE, MEDIO_PASSIVE, Some(PAST),    Some(PL_3)) -> "kallaðisk"
  ))

  @Test
  def testJShortStemVerja(): Unit = diff(WEAK_J_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "varj",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "varð",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "varð",
      EnumVerbStem.PERFECT_STEM -> "varð"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "verja",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "varðr",  // "variðr",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "verr",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "verjum",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "verið",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "verja",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "varða",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "varði",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "vǫrðum",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "vǫrðu",

      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "veri",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "verim",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "verið",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "veri",

      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "verða",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "verði",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "verðim",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "verði"
  ))

  @Test
  def testJLongStemFella(): Unit = diff(WEAK_J_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "falli",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "falld",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "falld",
      EnumVerbStem.PERFECT_STEM -> "falld"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "fella",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "felldr",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "fellir",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "fellum",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "fellið",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "fella",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "fellda",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "felldi",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "felldum",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "felldu",

      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "felli",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "fellim",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "fellið",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "felli",

      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "fellda",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "felldi",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "felldim",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "felldi"
  ))

  /*
    Basically the same test as that for telja, except the past stem is I-umlauted
   */
  @Test
  def testJShortStemSelja(): Unit = diff(WEAK_J_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "salj",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "sald",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "sald",
      EnumVerbStem.PERFECT_STEM -> "sald"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "selja",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "selr",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "saldi" //"seldi"
    ))

  @Test
  def testJShortStemTelja(): Unit = diff(WEAK_J_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "talj",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "tald",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "tald",
      EnumVerbStem.PERFECT_STEM -> "tald"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "telja",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "taldr",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "telr",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "teljum",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "telið",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "telja",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "talda",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "taldi",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "tǫldum",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "tǫldu",

      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_2)) -> "telir",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "teli",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "telim",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "telið",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "teli",

      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "telda",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "teldi",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "teldim",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "teldi"
    ))

  @Test
  def testIStemVaka(): Unit = diff(WEAK_I_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "vaki",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "vakt",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "vakt",
      EnumVerbStem.PERFECT_STEM -> "vakt"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "vaka",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "vaktr",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "vakir",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "vǫkum",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "vakið",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "vaka",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "vakta",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "vakti",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "vǫktum",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "vǫktu",

      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "vaki",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "vakim",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "vakið",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "vaki",

      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "vekta",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "vekti",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "vektim",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "vekti"
  ))
}
