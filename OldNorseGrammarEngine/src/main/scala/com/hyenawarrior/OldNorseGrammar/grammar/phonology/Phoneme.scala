package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Default

/**
  * Created by HyenaWarrior on 2018.09.21..
  */
trait Phoneme {

  def asShortened: Option[Phoneme]
  def asLengthened: Option[Phoneme]

  def isVowel: Boolean = false
  def isConsonant: Boolean = false

  def lengthInLetters: Int

  def asString: String

  def copyWithPropertyOf(phonemeProperty: PhonemeProperty): Phoneme

  def phonemeProperty: PhonemeProperty
}

object Phoneme {

  def extract(str: String): Phoneme = {

    if(str.length > 1) {

      val isDiphtong = Vowel2.allDiphtongs().contains(str substring 2)
      if (isDiphtong) {

        return Diphtong(str substring 2, Default)
      }
    }

    val isRegularVowel = Vowel2.allVowels().contains(str.head)
    if(isRegularVowel) {

      return SimpleVowel(str.head, Default)
    }

    val isCons = Consonant.isConsonant(str.head)
    if(isCons) {

      return Consonant(str.head.toString, Default)
    }

    throw new IllegalStateException()
  }
}
