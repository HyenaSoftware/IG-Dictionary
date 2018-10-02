package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Syllables.syllablesOf

import scala.language.{implicitConversions, postfixOps}

/**
  * Created by HyenaWarrior on 2017.03.01..
  */
class Word(val stringRepr: String) {

  def traditionalSyllables(): List[Syllable] = syllablesOf(stringRepr)
}

object Word {

  def apply(str: String) = new Word(str)

  @deprecated("Syllables.unapply will be deprecated")
  def unapply(word: Word): Option[List[Syllable]] = Syllables
    .unapply(word.stringRepr + "a")
    .map { syllables =>

      val Syllable(onset, _, "", isStressed, length) = syllables.last

      syllables.init :+ Syllable(onset, "", "", isStressed, length)
    }
}