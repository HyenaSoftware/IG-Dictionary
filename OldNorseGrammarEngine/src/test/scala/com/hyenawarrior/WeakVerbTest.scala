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
      EnumVerbStem.PRESENT_STEM -> "kall",
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
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "kallaði"
  ))

  @Test
  def testIShortStemVerja(): Unit = diff(WEAK_I_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "var",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "varð",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "varð",
      EnumVerbStem.PERFECT_STEM -> "varð"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "verja",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "variðr",

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
  def testILongStemFella(): Unit = diff(WEAK_I_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "fell",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "fellð",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "fellð",
      EnumVerbStem.PERFECT_STEM -> "fellð"
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

  @Test
  def testJStemVaka(): Unit = diff(WEAK_J_STEM
    , Map(
      EnumVerbStem.PRESENT_STEM -> "vak",
      EnumVerbStem.PRETERITE_SINGULAR_STEM -> "vakð",
      EnumVerbStem.PRETERITE_PLURAL_STEM -> "vakð",
      EnumVerbStem.PERFECT_STEM -> "vakð"
    )
    , Map(
      (INFINITIVE, ACTIVE, None,          None)       -> "vaka",
      (PARTICIPLE, ACTIVE, Some(PAST),    None)       -> "vakat",

      (INDICATIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "vakir",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "vǫkum",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "vakið",
      (INDICATIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "vaka",

      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "vakða",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "vakði",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "vǫkðum",
      (INDICATIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "vakðu",

      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(SG_3)) -> "vaki",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_1)) -> "vakim",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_2)) -> "vakið",
      (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(PL_3)) -> "vaki",

      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_1)) -> "vekða",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(SG_3)) -> "vekði",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_1)) -> "vekðim",
      (SUBJUNCTIVE, ACTIVE, Some(PAST),    Some(PL_3)) -> "vekði"
  ))
}
