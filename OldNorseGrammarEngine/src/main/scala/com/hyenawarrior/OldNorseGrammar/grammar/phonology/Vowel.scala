package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
	* Created by HyenaWarrior on 2017.10.15..
	*/
object Vowel {

	private val VOWELS = "aáeéiíoóöuú"
	private val SEMIVOWELS = "jv"

	private val raising = Map('e' -> 'i', 'o' -> 'u')

	def isVowel(c: Char): Boolean = VOWELS contains c

	def isSemivowel(c: Char): Boolean = SEMIVOWELS contains c

	def isVowelOrSemivowel(c: Char): Boolean = isSemivowel(c) || isVowel(c)

	def raise(c: Char): Char = raising(c)
}
