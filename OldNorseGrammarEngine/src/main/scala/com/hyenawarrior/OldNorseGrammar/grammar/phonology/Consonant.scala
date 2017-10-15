package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
	* Created by HyenaWarrior on 2017.10.15..
	*/
object Consonant {

	private val nasals				= Set('n', 'm')
	private val voicedStop		= Set('b', 'd', 'g')
	private val voicelessStop = Set('p', 't', 'k')
	private val devoicing 		= Map('b' -> 'p', 'd' -> 't', 'g' -> 'k')

	def isNasal(c: Char): Boolean = nasals contains c

	def isVoicedStop(c: Char): Boolean = voicedStop contains c

	def isVoicelessStop(c: Char): Boolean = voicelessStop contains c

	def devoice(c: Char): Char = devoicing(c)
}
