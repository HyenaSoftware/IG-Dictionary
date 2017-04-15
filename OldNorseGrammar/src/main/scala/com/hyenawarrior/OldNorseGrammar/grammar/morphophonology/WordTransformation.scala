package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait WordTransformation {

	final def apply(syllables: List[Syllable]): List[Syllable] =
	{
		if(isEligible(syllables)) forceApply(syllables) else syllables
	}

	def forceApply(syllables: List[Syllable]): List[Syllable]

	protected def isEligible(syllables: List[Syllable]): Boolean
}
