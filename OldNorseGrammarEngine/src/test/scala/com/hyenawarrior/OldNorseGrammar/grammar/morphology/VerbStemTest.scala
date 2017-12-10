package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Ablaut
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.Breaking
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.TransformationMode.EnabledFor
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.10.20..
	*/
class VerbStemTest {

	@Test
	def testAblautExtractionForSemivowels(): Unit = {

		val ablautGrade = Ablaut.getAblautGradeFrom("hjalpa")

		assertEquals("ja", ablautGrade.rootVowel)
	}

  @Test
  def testVerbStemClass2Extraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("skjót", STRONG_2ND_CLASS, PRESENT_STEM)

    val StrongVerbStem(root, _, _, _) = stemObj

    assertEquals("skjút", root.toString)
  }

  @Test
  def testVerbStemClass2(): Unit = {

    val stemObj = StrongVerbStem("skjút", STRONG_2ND_CLASS, PRESENT_STEM)

    // stem will be decaying to...
    assertEquals("skjót", stemObj.stringForm())
  }

  @Test
  def testVerbStemClass3Extraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("hjalp", STRONG_3RD_CLASS, PRESENT_STEM)

    val StrongVerbStem(root, _, _, _) = stemObj

    assertEquals("help", root.toString)
  }

	@Test
	def testVerbStemClass3(): Unit = {

    val stemObj = StrongVerbStem("help", STRONG_3RD_CLASS, PRESENT_STEM, EnabledFor(Breaking))

    // stem will be decaying to...
    assertEquals("hjalp", stemObj.stringForm())
	}

  @Test
  def testVerbStemClass3SongvaExtraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("singv", STRONG_3RD_CLASS, PRESENT_STEM)

    val StrongVerbStem(root, _, _, _) = stemObj

    assertEquals("sengv", root.toString)
  }

  @Test
  def testVerbStemClass3Songva(): Unit = {

    val stemObj = StrongVerbStem("sengv", STRONG_3RD_CLASS, PRESENT_STEM, EnabledFor(Breaking))

    // stem will be decaying to...
    assertEquals("singv", stemObj.stringForm())
  }

  /** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
  @Test
  def testVerbClass3rdDoNotRaise(): Unit = {

    val presentStemObj = StrongVerbStem("verð",  STRONG_3RD_CLASS, PRESENT_STEM)

    assertEquals("verð", 	presentStemObj.stringForm())
  }

	@Test
	def testVerbClass3rdNasalAssimilation(): Unit = {

		val presentStemObj = StrongVerbStem("band",  STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)

		assertEquals("batt", 	presentStemObj.stringForm())
	}

  @Test
  def testVerbClass3rdDevoiceAfterLateral(): Unit = {

    val stemObj = StrongVerbStem("gald",  STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)

    assertEquals("galt", 	stemObj.stringForm())
  }

  @Test
  def testVerbClass3rdDevoiceAfterLateralExtract(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("galt",  STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)

    val Root(rootRepr) = stemObj.getRoot()

    assertEquals("geld", rootRepr)
  }

  @Test
  def testVerbClass3rdAdaptiveNonProductiveRules(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("brenn",  STRONG_3RD_CLASS, PRESENT_STEM)

    val StrongVerbStem(normalizedStem, STRONG_3RD_CLASS, PRESENT_STEM, tMode) = stemObj
    assertEquals("brenn", normalizedStem)

    val stemObj2 = StrongVerbStem(normalizedStem, STRONG_3RD_CLASS, PRESENT_STEM, tMode)
    assertEquals("brenn", stemObj2.stringForm())
  }

  @Test
  def testVerbClass5thAblaut(): Unit = {

    val presentStemObj = StrongVerbStem("beðj", STRONG_5TH_CLASS, PRESENT_STEM)
    assertEquals("biðj", 	presentStemObj.stringForm())

    val pretSgStemObj = StrongVerbStem("baðj", STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM)
    assertEquals("baðj", 	pretSgStemObj.stringForm())

    val pretPlStemObj = StrongVerbStem("báðj", STRONG_5TH_CLASS, PRETERITE_PLURAL_STEM)
    assertEquals("báðj", 	pretPlStemObj.stringForm())
  }

  @Test
  def testVerbClass5thAblautExtract(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("biðj", STRONG_5TH_CLASS, PRESENT_STEM)

    val Root(rootRepr) = stemObj.getRoot()

    assertEquals("beð", rootRepr)
  }

  @Test
  def testVerbClass5thDoNotRaise(): Unit = {

    val stemObj = StrongVerbStem("veg", STRONG_5TH_CLASS, PRESENT_STEM)
    assertEquals("veg", stemObj.stringForm())
  }

  @Test
  def testVerbClass5thFinalGReductionAtJAugmentedStems(): Unit = {

    val root = Root("leg")

    val presentStemObj = StrongVerbStem.fromRoot(root, STRONG_5TH_CLASS, PRESENT_STEM)
    assertEquals("leg", presentStemObj.stringForm())

    val pretSgStemObj = StrongVerbStem.fromRoot(root, STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM)
    assertEquals("lá", 	pretSgStemObj.stringForm())

    val pretPlStemObj = StrongVerbStem.fromRoot(root, STRONG_5TH_CLASS, PRETERITE_PLURAL_STEM)
    assertEquals("lág", pretPlStemObj.stringForm())

    val perfectStemObj = StrongVerbStem.fromRoot(root, STRONG_5TH_CLASS, PERFECT_STEM)
    assertEquals("leg", perfectStemObj.stringForm())
  }

  @Test
  def testVerbClass5thShouldNotFinalGReductionAtJAugmentedStems(): Unit = {

    val normalizedStem = "gef"

    val presentStemObj = StrongVerbStem(normalizedStem, STRONG_5TH_CLASS, PRESENT_STEM)
    assertEquals("gef", presentStemObj.stringForm())

    val pretSgStemObj = StrongVerbStem.fromRoot(presentStemObj.getRoot(), STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM)
    assertEquals("gaf", 	pretSgStemObj.stringForm())

    val pretPlStemObj = StrongVerbStem.fromRoot(presentStemObj.getRoot(), STRONG_5TH_CLASS, PRETERITE_PLURAL_STEM)
    assertEquals("gáf", pretPlStemObj.stringForm())

    val perfectStemObj = StrongVerbStem.fromRoot(presentStemObj.getRoot(), STRONG_5TH_CLASS, PERFECT_STEM)
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

    val presentStemObj = StrongVerbStem.fromStrRepr("liggj", STRONG_5TH_CLASS, PRESENT_STEM)
    val Root(r1) = presentStemObj.getRoot()
    assertEquals("leg", r1)

    val pretSgStemObj = StrongVerbStem.fromStrRepr("lá", STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM)
    val Root(r2) = pretSgStemObj.getRoot()
    assertEquals("leg", 	r2)

    val pretPlStemObj = StrongVerbStem.fromStrRepr("lág", STRONG_5TH_CLASS, PRETERITE_PLURAL_STEM)
    val Root(r3) = pretPlStemObj.getRoot()
    assertEquals("leg", r3)

    val perfectStemObj = StrongVerbStem.fromStrRepr("leg", STRONG_5TH_CLASS, PERFECT_STEM)
    val Root(r4) = perfectStemObj.getRoot()
    assertEquals("leg", r4)
  }

  @Test
  def testVerbClass7th(): Unit = {

    val presentStemObj = StrongVerbStem.fromStrRepr("heit", STRONG_7_1_CLASS, PRESENT_STEM)

    assertEquals("heit", presentStemObj.stringForm())
  }
}
