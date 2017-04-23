package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Syllables.isVowel

/**
	* Created by HyenaWarrior on 2017.03.24..
	*/
case class Root(word: String)
{
	override def toString: String = word

	def rootVowel: String = {
		val Syllables(syllables) = word

		syllables.head.letters.filter(isVowel)
	}
}
