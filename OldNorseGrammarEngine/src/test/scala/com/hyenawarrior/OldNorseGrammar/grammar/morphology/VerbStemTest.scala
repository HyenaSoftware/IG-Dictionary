package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Ablaut
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.Breaking
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.TransformationMode.EnabledFor
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import org.junit.Assert._
import org.junit.{Assert, Test}

/**
	* Created by HyenaWarrior on 2017.10.20..
	*/
class VerbStemTest {

	@Test
	def testAblautExtractionForSemivowels(): Unit = {

		val ablautGrade = Ablaut.getAblautGradeFrom("hjalpa")

		Assert.assertEquals("ja", ablautGrade.rootVowel)
	}

  @Test
  def testVerbStemClass2Extraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("skjót", VerbClassEnum.STRONG_2ND_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(root, _, _, _) = stemObj

    Assert.assertEquals("skjút", root.toString)
  }

  @Test
  def testVerbStemClass2(): Unit = {

    val stemObj = StrongVerbStem("skjút", VerbClassEnum.STRONG_2ND_CLASS, EnumVerbStem.PRESENT_STEM)

    // stem will be decaying to...
    Assert.assertEquals("skjót", stemObj.stringForm())
  }

  @Test
  def testVerbStemClass3Extraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("hjalp", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(root, _, _, _) = stemObj

    Assert.assertEquals("help", root.toString)
  }

	@Test
	def testVerbStemClass3(): Unit = {

    val stemObj = StrongVerbStem("help", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM, EnabledFor(Breaking))

    val StrongVerbStem(root, _, _, _) = stemObj

    // stem will be decaying to...
    Assert.assertEquals("hjalp", stemObj.stringForm())
	}

  @Test
  def testVerbStemClass3SongvaExtraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("singv", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(root, _, _, _) = stemObj

    Assert.assertEquals("sengv", root.toString)
  }

  @Test
  def testVerbStemClass3Songva(): Unit = {

    val stemObj = StrongVerbStem("sengv", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM, EnabledFor(Breaking))

    val StrongVerbStem(root, _, _, _) = stemObj

    // stem will be decaying to...
    Assert.assertEquals("singv", stemObj.stringForm())
  }

  /** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
  @Test
  def testVerbClass3rdDoNotRaise(): Unit = {

    val presentStemObj = StrongVerbStem("verð", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    assertEquals("verð", 	presentStemObj.stringForm())
  }

	@Test
	def testVerbClass3rdNasalAssimilation(): Unit = {

		val presentStemObj = StrongVerbStem("band", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

		assertEquals("batt", 	presentStemObj.stringForm())
	}

  @Test
  def testVerbClass3rdDevoiceAfterLateral(): Unit = {

    val stemObj = StrongVerbStem("geld", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

    assertEquals("galt", 	stemObj.stringForm())
  }

  @Test
  def testVerbClass3rdDevoiceAfterLateralExtract(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("galt", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

    val Root(rootRepr) = stemObj.getRoot()

    Assert.assertEquals("geld", rootRepr)
  }

  @Test
  def testVerbClass3rdAdaptiveNonProductiveRules(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("brenn", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(normalizedStem, VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM, tMode) = stemObj
    assertEquals("brenn", normalizedStem)

    val stemObj2 = StrongVerbStem(normalizedStem,  VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM, tMode)
    assertEquals("brenn", stemObj2.stringForm())
  }

  @Test
  def testVerbClass5thAblaut(): Unit = {

    val presentStemObj = StrongVerbStem("beð", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRESENT_STEM)
    assertEquals("beð", 	presentStemObj.stringForm())

    val pretSgStemObj = StrongVerbStem("beð", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
    assertEquals("bað", 	pretSgStemObj.stringForm())

    val pretPlStemObj = StrongVerbStem("beð", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
    assertEquals("báð", 	pretPlStemObj.stringForm())
  }

  @Test
  def testVerbClass5thAblautExtract(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("biðj", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRESENT_STEM)

    val Root(rootRepr) = stemObj.getRoot()

    Assert.assertEquals("beð", rootRepr)
  }

  @Test
  def testVerbClass5thDoNotRaise(): Unit = {

    val stemObj = StrongVerbStem("veg", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRESENT_STEM)
    assertEquals("veg", stemObj.stringForm())
  }

  @Test
  def testVerbClass5thFinalGReductionAtJAugmentedStems(): Unit = {

    val root = "leg"

    val presentStemObj = StrongVerbStem(root, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRESENT_STEM)
    assertEquals("leg", presentStemObj.stringForm())

    val pretSgStemObj = StrongVerbStem(root, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
    assertEquals("lá", 	pretSgStemObj.stringForm())

    val pretPlStemObj = StrongVerbStem(root, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
    assertEquals("lág", pretPlStemObj.stringForm())

    val perfectStemObj = StrongVerbStem(root, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PERFECT_STEM)
    assertEquals("leg", perfectStemObj.stringForm())
  }

  @Test
  def testVerbClass5thShouldNotFinalGReductionAtJAugmentedStems(): Unit = {

    val normalizedStem = "gef"

    val presentStemObj = StrongVerbStem(normalizedStem, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRESENT_STEM)
    assertEquals("gef", presentStemObj.stringForm())

    val pretSgStemObj = StrongVerbStem(normalizedStem, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
    assertEquals("gaf", 	pretSgStemObj.stringForm())

    val pretPlStemObj = StrongVerbStem(normalizedStem, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
    assertEquals("gáf", pretPlStemObj.stringForm())

    val perfectStemObj = StrongVerbStem(normalizedStem, VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PERFECT_STEM)
    assertEquals("gef", perfectStemObj.stringForm())
  }

  /*
      at past forms it's uncertain what would be the correct stem

           verb decayed stem  unstable stem
      sg3  lá   lá-           laggj-
      sg3  vá   vá-           vag-

      pl3  lágu lág-          lággj-
      pl3  vágu vág-          vág-

      Why we have the a) case, but not the b) ?

      a)
        present stem      infinitive
        *beðj- > biðj-    biðja
        *leggj- > liggj-  liggja
        veg-              vega

      b)
        beð-              beða
        leg-              lega
        *vaggj- > viggj-  viggja
   */
  @Test
  def testVerbClass5thInverseOfFinalGReductionAtJAugmentedStems(): Unit = {

    val presentStemObj = StrongVerbStem.fromStrRepr("liggj", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRESENT_STEM)
    val Root(r1) = presentStemObj.getRoot()
    assertEquals("leg", r1)

    val pretSgStemObj = StrongVerbStem.fromStrRepr("lá", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
    val Root(r2) = pretSgStemObj.getRoot()
    assertEquals("leg", 	r2)

    val pretPlStemObj = StrongVerbStem.fromStrRepr("lág", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
    val Root(r3) = pretPlStemObj.getRoot()
    assertEquals("leg", r3)

    val perfectStemObj = StrongVerbStem.fromStrRepr("leg", VerbClassEnum.STRONG_5TH_CLASS, EnumVerbStem.PERFECT_STEM)
    val Root(r4) = perfectStemObj.getRoot()
    assertEquals("leg", r4)
  }
}
