package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphology._
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
	classOf[NounFormTest],
	classOf[NounStemTest],
	classOf[SoundexTest],
	classOf[TestTransformation],
  classOf[TestVowels],
	classOf[VerbStemTest],
	classOf[VerbTest],
	classOf[SieversLawTest],
	classOf[SubjVerbTest],
	classOf[SyncopeTest],
	classOf[SyllablificationTest],
  classOf[MediopassiveVerbTest],
	classOf[SubjMediopassiveVerbTest],
	classOf[UmlautTest],
	classOf[WeakVerbTest]
))
class TestSuite
