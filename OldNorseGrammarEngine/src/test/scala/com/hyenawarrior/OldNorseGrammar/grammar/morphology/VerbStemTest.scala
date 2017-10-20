package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Ablaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
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
	def testVerbStemClass3(): Unit = try {

		val stemObj = StrongVerbStem.fromStrRepr("hjalp", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

	} catch {

		case e: RuntimeException => Assert.fail(e.getMessage)
	}
}
