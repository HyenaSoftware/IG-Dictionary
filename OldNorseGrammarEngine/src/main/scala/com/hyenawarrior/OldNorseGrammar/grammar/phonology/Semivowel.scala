package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
  * Created by HyenaWarrior on 2018.06.09..
  */
final case class Semivowel(c: Char, phonemeProperty: PhonemeProperty) extends Phoneme {

  override def asShortened: Option[Phoneme] = ???

  override def asLengthened: Option[Phoneme] = ???

  override def copyWithPropertyOf(phonemeProperty: PhonemeProperty) = Semivowel(c, phonemeProperty)

  override val lengthInLetters: Int = 1

  override val asString: String = c.toString
}
