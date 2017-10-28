package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Ablaut
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

    val StrongVerbStem(root, _, _) = stemObj

    Assert.assertEquals("skjút", root.toString)
  }

  @Test
  def testVerbStemClass2(): Unit = {

    val stemObj = StrongVerbStem(Root("skjút"), VerbClassEnum.STRONG_2ND_CLASS, EnumVerbStem.PRESENT_STEM)

    // stem will be decaying to...
    Assert.assertEquals("skjót", stemObj.stringForm())
  }

  @Test
  def testVerbStemClass3Extraction(): Unit = {

    val stemObj = StrongVerbStem.fromStrRepr("hjalp", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(root, _, _) = stemObj

    Assert.assertEquals("help", root.toString)
  }

	@Test
	def testVerbStemClass3(): Unit = {

    val stemObj = StrongVerbStem(Root("help"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(root, _, _) = stemObj

    // stem will be decaying to...
    Assert.assertEquals("hjalp", stemObj.stringForm())
	}

  @Test
  def testVerbStemClass3Songva(): Unit = {

    val stemObj = StrongVerbStem(Root("sengv"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    val StrongVerbStem(root, _, _) = stemObj

    // stem will be decaying to...
    Assert.assertEquals("singv", stemObj.stringForm())
  }

  /** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
  @Test
  def testClass3rdFromDoNotChangeStemToJa(): Unit = {

    val presentStemObj = StrongVerbStem(Root("verð"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

    assertEquals("verð", 	presentStemObj.stringForm())
  }
}
