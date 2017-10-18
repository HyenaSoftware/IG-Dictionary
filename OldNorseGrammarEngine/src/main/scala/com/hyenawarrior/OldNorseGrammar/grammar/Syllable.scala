package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowel

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
case class Syllable(letters: String, stressed: Boolean)
{
	def isStressed = stressed

	def nucleus: String = letters.filter(isVowel)
}
