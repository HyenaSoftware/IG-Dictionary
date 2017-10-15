package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowel

/**
	* Created by HyenaWarrior on 2017.03.24..
	*/
case class Root(word: String)
{
	/**
		.toString() is the way to get the stringrepresentation
	 */
	override def toString: String = word

	def rootVowel: String = {

		val Syllables(syllables) = word

		syllables.head.letters.filter(isVowel)
	}
}
