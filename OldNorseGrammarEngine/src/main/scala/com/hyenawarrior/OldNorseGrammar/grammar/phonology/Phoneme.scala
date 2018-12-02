package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Default

import scala.collection.mutable

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

  def asStringRepr(phs: Seq[Phoneme]): String = {

    val lb = new mutable.ListBuffer[Char]

    phs.foreach {

      case Consonant(c, _) => lb += c
      case HalfDigraph(c, FirstDigraphLetter,   _) => lb ++= s"${c}1"
      case HalfDigraph(c, SecondDigraphLetter,  _) => lb ++= s"${c}2"
      case Diphtong(c2, _) => lb ++= c2
      case SimpleVowel(c, _) => lb += c
      case Semivowel(c, _) => lb += c
    }

    lb.mkString
  }

  def fromStringRepr(str: String, stemProperty: MorphemeProperty): SimpleMorpheme = {

    var i = 0

    val lb = new mutable.ListBuffer[Phoneme]

    while(i < str.length) {

      val ss = str substring i

      val (ph, l) = Phoneme extract ss

      i += l

      lb += ph
    }

    SimpleMorpheme(lb, stemProperty)
  }

  def extract(str: String): (Phoneme, Int) = {

    if(str.length > 1) {

      val c2 = str take 2
      val isDiphtong = Vowel2.allDiphtongs().contains(c2)
      if (isDiphtong) {

        return Diphtong(c2, Default) -> 2
      }
    }

    val isRegularVowel = Vowel2.allVowels().contains(str.head)
    if(isRegularVowel) {

      return SimpleVowel(str.head, Default) -> 1
    }

    if(str.startsWith("z1") || str.startsWith("x1")) {

      return HalfDigraph(str.head, FirstDigraphLetter, Default) -> 2
    }

    if(str.startsWith("z2") || str.startsWith("x2")) {

      return HalfDigraph(str.head, SecondDigraphLetter, Default) -> 2
    }

    val isCons = Consonant.isConsonant(str.head)
    if(isCons) {

      return Consonant(str.head, Default) -> 1
    }

    throw new IllegalStateException()
  }
}
