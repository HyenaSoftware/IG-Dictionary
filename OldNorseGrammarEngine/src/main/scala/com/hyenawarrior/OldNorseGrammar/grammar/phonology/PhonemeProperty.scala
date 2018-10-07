package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
  * Created by HyenaWarrior on 2018.09.22..
  */

trait PhonemeProperty

object PhonemeProperty {

  trait DisappearingPhoneme extends PhonemeProperty
  trait AppearingPhoneme extends PhonemeProperty
  trait AlteringPhoneme extends PhonemeProperty

  // semivowels, syncopated vowels, certain consonants
  // ríkija > ríkja, vulf- > úlf-

  object Syncopated extends DisappearingPhoneme { override def toString: String = "[Syncopated]" }

  // -áa- > -á-, -eu- > -jau- > -jó-
  object Merged extends PhonemeProperty

  object Default extends PhonemeProperty

  // lát > látt
  object Lengthtened extends PhonemeProperty

  object Shortened extends PhonemeProperty

  object Voiced extends PhonemeProperty

  object Devoiced extends PhonemeProperty

  object FrontMutated extends PhonemeProperty

  object BackMutated extends PhonemeProperty

  trait Umlauted extends AlteringPhoneme

  object U_Umlauted extends Umlauted
}