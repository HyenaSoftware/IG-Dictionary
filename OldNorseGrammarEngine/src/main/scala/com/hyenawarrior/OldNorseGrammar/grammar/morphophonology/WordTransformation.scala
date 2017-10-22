package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait WordTransformation {

	final def apply(str: String): String = {

    val Syllables(syllables) = str

    val newSyllables = apply(syllables)

    newSyllables.flatMap(_.letters).mkString
  }

	final def apply(syllables: List[Syllable]): List[Syllable] = {

		if(isEligible(syllables)) forceApply(syllables) else syllables
	}

	final def forceApply(str: String): String = {

		val Syllables(syllables) = str

		val newSyllables = forceApply(syllables)

    newSyllables.flatMap(_.letters).mkString
  }

	def forceApply(syllables: List[Syllable]): List[Syllable]

  def isEligible(syllables: List[Syllable]): Boolean
}

trait Explicit extends WordTransformation
{
	override def isEligible(syllables: List[Syllable]): Boolean = true
}
