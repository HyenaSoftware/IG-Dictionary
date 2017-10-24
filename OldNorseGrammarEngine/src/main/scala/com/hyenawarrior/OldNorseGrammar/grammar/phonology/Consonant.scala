package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
	* Created by HyenaWarrior on 2017.10.15..
	*/
object Consonant {

	private val CONSONANTS 		= Set('b', 'd', 'ð', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'r', 's', 't', 'x', 'þ')

	private val nasals				= Set('n', 'm')

	private val voicedStop		= Set('b', 'd', 'g')
	private val voicelessStop = Set('p', 't', 'k')

  private val dentals = Set('ð', 'd', 't', 's')

	private val devoicing 		= Map('b' -> 'p', 'd' -> 't', 'g' -> 'k')

	def isConsonant(c: Char) = CONSONANTS contains c

	def isNasal(c: Char): Boolean = nasals contains c

	def isVoicedStop(c: Char): Boolean = voicedStop contains c

	def isVoicelessStop(c: Char): Boolean = voicelessStop contains c

  def isDental(c: Char) = dentals contains c

	def devoice(c: Char): Char = devoicing(c)
}
