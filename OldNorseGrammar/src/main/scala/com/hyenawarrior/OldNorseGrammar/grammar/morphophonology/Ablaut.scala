package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._

/**
	* Created by HyenaWarrior on 2017.04.25..
	*/
case class Ablaut(presentVowel: String, preteriteSingularVowel: String, preteritePluralVowel: String, perfectVowel: String)
{
	val VOWELS = Map(
		PRESENT_STEM						-> AblautGrade(presentVowel),
		PRETERITE_SINGULAR_STEM	-> AblautGrade(preteriteSingularVowel),
		PRETERITE_PLURAL_STEM	 	-> AblautGrade(preteritePluralVowel),
		PERFECT_STEM						-> AblautGrade(perfectVowel)
	)
}
