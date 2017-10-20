package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.morphology.{UmlautTest, VerbStemTest}
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
	classOf[VerbStemTest],
	classOf[VerbTest],
	classOf[UmlautTest]
))
class TestSuite
{

}
