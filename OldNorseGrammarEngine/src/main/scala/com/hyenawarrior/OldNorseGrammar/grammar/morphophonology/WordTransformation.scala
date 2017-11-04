package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait WordTransformation {

	def apply(str: String): String = {

    val Syllables(syllables) = str

    val newSyllables = apply(syllables)

    newSyllables.flatMap(_.letters).mkString
  }

	def apply(syllables: List[Syllable]): List[Syllable]

  def canTransform(str: String): Boolean = str match {

    case Syllables(syllables) => canTransform (syllables)
    case _ => false
  }

  def canTransform(syllables: List[Syllable]): Boolean
}

trait Explicit extends WordTransformation
{
	override def canTransform(syllables: List[Syllable]): Boolean = true
}
