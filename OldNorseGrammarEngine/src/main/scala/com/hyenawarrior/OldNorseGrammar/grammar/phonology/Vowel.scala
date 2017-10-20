package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
	* Created by HyenaWarrior on 2017.10.15..
	*/
object Vowel {

	private val VOWELS = "aáeéiíøœoóöuúyý".toSet
	private val SEMIVOWELS = "jv"
	private val BACK_VOWELS		= Set('a', 'á',      'o', 'ó', 'ö', 'u', 'ú')
	private val LABIAL_VOWELS = Set(          'ø', 'o', 'ó', 'ö', 'u', 'ú', 'y', 'ý', 'œ')

	private val raising = Map('e' -> 'i', 'o' -> 'u')

	def isVowel(c: Char): Boolean = VOWELS contains c

	def isSemivowel(c: Char): Boolean = SEMIVOWELS contains c

	def isVowelOrSemivowel(c: Char): Boolean = isSemivowel(c) || isVowel(c)

	def raise(c: Char): Char = raising(c)

	def isBackVowel(c: Char): Boolean = BACK_VOWELS contains c

	def isLabialVowel(c: Char): Boolean = LABIAL_VOWELS contains c
}
