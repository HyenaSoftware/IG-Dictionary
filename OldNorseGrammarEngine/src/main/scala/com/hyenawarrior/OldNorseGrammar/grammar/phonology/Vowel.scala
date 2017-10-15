package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
	* Created by HyenaWarrior on 2017.10.15..
	*/
object Vowel {

	private val VOWELS = "aáeéiíoóöuú"
	private val raising = Map('e' -> 'i', 'o' -> 'u')

	def isVowel(c: Char): Boolean = VOWELS.exists(c == _)

	def raise(c: Char): Char = raising(c)
}
