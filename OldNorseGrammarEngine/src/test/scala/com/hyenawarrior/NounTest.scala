package com.hyenawarrior

import com.hyenawarrior.NounTest._
import com.hyenawarrior.NounTestAux.{diff, nonReversible}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounFormType, NounType}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses._
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
object NounTest {

  def defin(declension: NounType): NounFormType = declension -> true
  def indef(declension: NounType): NounFormType = declension -> false
}

class NounTest {

  @Test
  def testStrongMascWaStem(): Unit = {

    val noun = Noun(StrongStemClassMascA, Map(indef(SINGULAR -> NOMINATIVE) -> "sǫngr"))

    assertEquals("sǫng", 		(noun nounForms SINGULAR -> ACCUSATIVE -> false).strRepr)

    assertEquals("sǫngvar", (noun nounForms PLURAL -> NOMINATIVE -> false).strRepr)
    assertEquals("sǫngum", 	(noun nounForms PLURAL -> DATIVE -> false).strRepr)
  }

  @Test
  def testStrongMascIStem(): Unit = {

    val noun = Noun(StrongStemClassMascI, Map(indef(SINGULAR -> NOMINATIVE) -> "gestr"))

    assertEquals("gestum", (noun nounForms PLURAL -> DATIVE -> false).strRepr)
  }

  @Test
  def testStrongMascAForms(): Unit = diff(StrongStemClassMascA, Map(

    indef(SINGULAR, NOMINATIVE) -> "úlfr",
    indef(SINGULAR, ACCUSATIVE) -> "úlf",

    indef(PLURAL, NOMINATIVE) -> "úlfar",
    indef(PLURAL, ACCUSATIVE) -> "úlfa",
    indef(PLURAL, DATIVE) -> "úlfum",

    defin(PLURAL, NOMINATIVE) -> "úlfarnir",
    defin(PLURAL, DATIVE) -> "úlfunum"
  ))

  @Test
  def testStrongMascAForms2(): Unit = diff(StrongStemClassMascA, Map(

    indef(PLURAL, NOMINATIVE) -> "hestar",
    indef(PLURAL, ACCUSATIVE) -> "hesta",

    defin(PLURAL, NOMINATIVE) -> "hestarnir",
    defin(PLURAL, ACCUSATIVE) -> "hestana"
  ))

  @Test
  def testStrongNeuterAForms(): Unit =  diff(StrongStemClassNeuter, Map(

    indef(SINGULAR, NOMINATIVE) -> "kné",
    indef(SINGULAR, GENITIVE)   -> "knés",

    indef(PLURAL,   NOMINATIVE) -> "kné",
    indef(PLURAL,   GENITIVE)   -> "knjá",
    indef(PLURAL,   DATIVE)     -> "knjám"
    //(PLURAL,   DATIVE)     -> "knjóm"
  ))

  @Test
  def testStrongNeuterJaForms(): Unit =  diff(StrongStemClassNeuter, Map(

    indef(SINGULAR, NOMINATIVE) -> "ríki",
    indef(SINGULAR, GENITIVE) -> "ríkis",

    indef(PLURAL, ACCUSATIVE) -> "ríki",
    indef(PLURAL, GENITIVE) -> "ríkja",
    indef(PLURAL, DATIVE) -> "ríkjum"
  ))

  @Test
  def testStrongNeuterAForms2(): Unit =  diff(StrongStemClassNeuter, Map(

    indef(SINGULAR, NOMINATIVE) -> "land",
    indef(SINGULAR, GENITIVE) -> "lands",

    indef(PLURAL, ACCUSATIVE) -> "lǫnd",
    indef(PLURAL, GENITIVE) -> "landa",
    indef(PLURAL, DATIVE) -> "lǫndum"
  ))

  @Test
  def testStrongNeuterAForms3(): Unit =  diff(StrongStemClassNeuter, Map(

    indef(SINGULAR, NOMINATIVE) -> "tré",
    indef(SINGULAR, GENITIVE) -> "trés",

    indef(PLURAL, ACCUSATIVE) -> "tré",
    indef(PLURAL, GENITIVE) -> "trjáa",
    indef(PLURAL, DATIVE) -> "trjám",

    defin(SINGULAR, NOMINATIVE) -> "tréit",
    defin(SINGULAR, GENITIVE) -> "trésins",

    defin(PLURAL, ACCUSATIVE) -> "tréin",
    defin(PLURAL, GENITIVE) -> "trjánna",
    defin(PLURAL, DATIVE) -> "trjánum"
  ))

  @Test
  def testStrongNeuterWaForms(): Unit =  diff(StrongStemClassNeuter, Map(

    indef(SINGULAR, NOMINATIVE) -> "hǫgg",
    indef(SINGULAR, ACCUSATIVE) -> "hǫgg",
    indef(SINGULAR, DATIVE) -> "hǫggvi",

    indef(PLURAL, NOMINATIVE) -> "hǫgg",
    indef(PLURAL, GENITIVE) -> "hǫggva",
    indef(PLURAL, DATIVE) -> "hǫggum"
  ))

  @Test
  def testStrongFeminineJoFormsLong(): Unit =  diff(StrongStemClassFeminineA1, Map(

    indef(SINGULAR, NOMINATIVE) -> "heiðr",
    indef(SINGULAR, GENITIVE) -> "heiðar",

    indef(PLURAL, NOMINATIVE) -> "heiðar",
    indef(PLURAL, GENITIVE) -> "heiða",
    indef(PLURAL, DATIVE) -> "heiðum"
  ))

  @Test
  def testStrongFeminineOForms(): Unit =  diff(StrongStemClassFeminineA2, Map(

    indef(SINGULAR, NOMINATIVE) -> "grǫf",
    indef(SINGULAR, GENITIVE) -> "grafar",

    indef(PLURAL, NOMINATIVE) -> "grafar",
    indef(PLURAL, GENITIVE) -> "grafa",
    indef(PLURAL, DATIVE) -> "grǫfum"
  ))

  @Test
  def testStrongFeminineOForms2(): Unit =  diff(StrongStemClassFeminineA2, Map(

    indef(SINGULAR, NOMINATIVE) -> "gjǫf",
    indef(SINGULAR, GENITIVE) -> "gjafar",

    indef(PLURAL, NOMINATIVE) -> "gjafar",
    indef(PLURAL, GENITIVE) -> "gjafa",
    indef(PLURAL, DATIVE) -> "gjǫfum",

    defin(SINGULAR, NOMINATIVE) -> "gjǫfin",
    defin(SINGULAR, ACCUSATIVE) -> "gjǫfina",
    defin(SINGULAR, DATIVE) -> "gjǫfinni",
    defin(SINGULAR, GENITIVE) -> "gjafarinnar",

    defin(PLURAL, NOMINATIVE) -> "gjafarnar",
    defin(PLURAL, DATIVE) -> "gjǫfunum",
    defin(PLURAL, GENITIVE) -> "gjafanna"
  ))

  @Test
  def testStrongFeminineJoForms(): Unit =  diff(StrongStemClassFeminineA2, Map(

    indef(SINGULAR, NOMINATIVE) -> "ey",
    indef(SINGULAR, DATIVE) -> "eyju",
    indef(SINGULAR, GENITIVE) -> "eyjar",

    indef(PLURAL, NOMINATIVE) -> "eyjar",
    indef(PLURAL, GENITIVE) -> "eyja",
    indef(PLURAL, DATIVE) -> "eyjum"
  ))

  @Test
  def testStrongFeminineWoForms(): Unit =  diff(StrongStemClassFeminineA2, Map(

    indef(SINGULAR, NOMINATIVE)  -> nonReversible("ǫr"),
    indef(SINGULAR, DATIVE)      -> nonReversible("ǫr"),
    indef(SINGULAR, GENITIVE)    -> "ǫrvar",

    indef(PLURAL, NOMINATIVE)  -> "ǫrvar",
    indef(PLURAL, GENITIVE)    -> "ǫrva",
    indef(PLURAL, DATIVE)      -> nonReversible("ǫrum")
  ))

  @Test
  def testStrongMascIForms(): Unit =  diff(StrongStemClassMascI, Map(

    indef(SINGULAR, NOMINATIVE)  -> "gestr",
    indef(SINGULAR, DATIVE)      -> "gest",
    indef(SINGULAR, GENITIVE)    -> "gests",

    indef(PLURAL, NOMINATIVE)  -> "gestir",
    indef(PLURAL, GENITIVE)    -> "gesta",
    indef(PLURAL, DATIVE)      -> "gestum"
  ))

  @Test
  def testStrongFeminineIForms(): Unit =  diff(StrongStemClassFeminineI, Map(

    indef(SINGULAR, NOMINATIVE)  -> "hǫll",
    indef(SINGULAR, DATIVE)      -> "hǫll",
    indef(SINGULAR, GENITIVE)    -> "hallar",

    indef(PLURAL, NOMINATIVE)  -> "hallir",
    indef(PLURAL, GENITIVE)    -> "halla",
    indef(PLURAL, DATIVE)      -> "hǫllum"
  ))

  @Test
  def testStrongMascRForms(): Unit =  diff(StrongStemClassMascR, Map(

    indef(SINGULAR, NOMINATIVE)  -> "maðr",
    indef(SINGULAR, ACCUSATIVE)  -> nonReversible("mann"),
    indef(SINGULAR, GENITIVE)    -> "manns",

    indef(PLURAL, NOMINATIVE)  -> nonReversible("menn"),
    indef(PLURAL, GENITIVE)    -> "manna",
    indef(PLURAL, DATIVE)      -> "mǫnnum"
  ))

  @Test
  def testStrongFeminineRForms(): Unit =  diff(StrongStemClassFeminineR, Map(

    indef(SINGULAR, NOMINATIVE)  -> "strǫnd",
    indef(SINGULAR, GENITIVE)    -> "strandar",

    indef(PLURAL, NOMINATIVE)  -> "strendr",
    indef(PLURAL, GENITIVE)    -> "stranda",
    indef(PLURAL, DATIVE)      -> "strǫndum"
  ))

  @Test
  def testStrongMascUForms(): Unit =  diff(StrongStemClassMascU, Map(

    indef(SINGULAR, NOMINATIVE)  -> "vǫllr",
    indef(SINGULAR, ACCUSATIVE)  -> "vǫll",
    indef(SINGULAR, DATIVE)      -> "velli",
    indef(SINGULAR, GENITIVE)    -> "vallar",

    indef(PLURAL, NOMINATIVE)  -> "vellir",
    indef(PLURAL, DATIVE)      -> "vǫllum",
    indef(PLURAL, GENITIVE)    -> "valla"
  ))

  // in/weak fem I-stem
  @Test
  def testWeakFemIForms(): Unit =  diff(WeakStemClassFeminineI, Map(

    indef(SINGULAR, NOMINATIVE)  -> "gleði",
    indef(SINGULAR, ACCUSATIVE)  -> "gleði",

    indef(PLURAL, NOMINATIVE)  -> "gleðar",
    indef(PLURAL, GENITIVE)    -> "gleða",
    indef(PLURAL, DATIVE)      -> "gleðum"
  ))

  // on/jon weak feminine U-stem
  @Test
  def testWeakFemUForms(): Unit =  diff(WeakStemClassFeminineU, Map(

    indef(SINGULAR, NOMINATIVE)  -> "saga",
    indef(SINGULAR, ACCUSATIVE)  -> "sǫgu",

    indef(PLURAL, NOMINATIVE)  -> "sǫgur",
    indef(PLURAL, GENITIVE)    -> "sagna"
  ))

  // an/jan weak masc A-stem
  @Test
  def testWeakMascAForms(): Unit =  diff(WeakStemClassMascA, Map(

    indef(SINGULAR, NOMINATIVE)  -> "bogi",
    indef(SINGULAR, ACCUSATIVE)  -> "boga",

    indef(PLURAL, NOMINATIVE)  -> "bogar",
    indef(PLURAL, GENITIVE)    -> "boga",
    indef(PLURAL, DATIVE)      -> "bogum"
  ))

  // -nd/weak masc R-stem
  @Test
  def testWeakMascRForms(): Unit =  diff(WeakStemClassMascR, Map(

    indef(SINGULAR, NOMINATIVE)  -> "bóndi",
    indef(SINGULAR, ACCUSATIVE)  -> "bónda",

    indef(PLURAL, NOMINATIVE)  -> "bœndr",
    indef(PLURAL, GENITIVE)    -> "bónda",
    indef(PLURAL, DATIVE)      -> "bóndum"
  ))

  // an/jan weak neuter
  @Test
  def testWeakNeuterForms(): Unit =  diff(WeakStemClassNeuter, Map(

    indef(SINGULAR, NOMINATIVE)  -> "hjarta",
    indef(SINGULAR, ACCUSATIVE)  -> "hjarta",

    indef(PLURAL, NOMINATIVE)  -> "hjǫrtu",
    indef(PLURAL, GENITIVE)    -> "hjartna",
    indef(PLURAL, DATIVE)      -> "hjǫrtum"
  ))
}
