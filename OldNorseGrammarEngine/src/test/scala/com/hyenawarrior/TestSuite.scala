package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.SubjVerbTest
import com.hyenawarrior.OldNorseGrammar.grammar.morphology.{TestTransformation, UmlautTest, VerbStemTest}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.TestVowels
import org.junit.runner.RunWith
import org.junit.runners.Suite


/**
	* Created by HyenaWarrior on 2017.08.29..
	*/
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[ExampleUnitTest],
	classOf[NounTest],
	classOf[SoundexTest],
	classOf[TestTransformation],
  classOf[TestVowels],
	classOf[VerbStemTest],
	classOf[VerbTest],
	classOf[SubjVerbTest],
	classOf[UmlautTest]
))
class TestSuite
