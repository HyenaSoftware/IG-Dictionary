package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import java.lang.Math.max

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.StemAndSuffix

/**
  * Created by HyenaWarrior on 2018.09.22..
  */
// less-defined, it can represent syllables
// can be split dynamically
case class Word(morphemes: Seq[SimpleMorpheme]) {

  /**
    * location  string
    * 0   -5    "[abcd"
    * 1   -4    "a[bcd"
    * 2   -3    "ab[cd"
    * 3   -2    "abc[d"
    * 4   -1    "abcd["
    *
    * but Scala API splitAt works like:
    * 0          "[abcd"
    *
    * @param location
    * @return
    */
  def splitAt(location: Int): Word = {

    splitAtTo(location) match {

      case Some((sq1, sq2)) => new Word(sq1 ++ sq2)
      case None => this
    }
  }

  def splitAtTo(location: Int): Option[(Seq[SimpleMorpheme], Seq[SimpleMorpheme])] = {

    var parsedLength = 0
    var morphemeIndex = -1

    val absSplitLocation = location + (if(location < 0) lengthInLetters+1 else 0)

    while(absSplitLocation > parsedLength) {

      morphemeIndex += 1

      val mh = morphemes(morphemeIndex)
      parsedLength = parsedLength + mh.lengthInLetters
    }

    val baseSimpleMorpheme = morphemes(max(0, morphemeIndex))
    val morphemeLength = morphemeIndex match {

      case -1 => 0
      case i => morphemes(i).lengthInLetters
    }

    val relativeOffset = absSplitLocation - (parsedLength - morphemeLength)

    (baseSimpleMorpheme splitAtToMorphemes relativeOffset).map {

      case (m1, m2) =>
        val (sq1, sq2) = morphemes splitAt morphemeIndex
        (sq1 :+ m1) -> (m2 +:sq2.tail)
    }
  }

  def enumerateMorphemes(f: SimpleMorpheme => Unit): Unit = {

    for(mh <- morphemes) {

      f(mh)
    }
  }

  def selectMorpheme(morphemeProperty: MorphemeProperty, i: Int = 1): Option[SimpleMorpheme] = morphemes
    .filter { _ is morphemeProperty }
    .take(i).lastOption

  def +(e: SimpleMorpheme): Word = new Word(morphemes :+ e)

  def +(other: Word): Word = new Word(morphemes ++ other.morphemes)

  type MorphemeTransformer = PartialFunction[(SimpleMorpheme, Int), SimpleMorpheme]

  def transformMorphemes(f: MorphemeTransformer): Word = {

    val newMhs = morphemes.zipWithIndex map {
      case ei if f isDefinedAt ei => f(ei)
      case (e, _) => e
    }

    new Word(newMhs)
  }

  def selectMorpheme(f: SimpleMorpheme => Boolean): Option[SimpleMorpheme] = morphemes.find(m => f(m))

  def lengthInLetters: Int = morphemes.map(_.lengthInLetters).sum

  def asString: String = morphemes.flatMap(_.asString).mkString

  def asString(filter: PhonemeProperty => Boolean): String = morphemes.flatMap(_.asString(filter)).mkString

  override def toString: String = morphemes.flatMap(mh => s"[$mh]").mkString
}

object Word {

  trait Location extends Any { def beforeThisIndex: Int }

  case class BeforeAt(index: Int) extends AnyVal with Location {

    override def beforeThisIndex: Int = index

    override def toString: String = s"before $index"
  }

  case class AfterAt(index: Int) extends AnyVal with Location {

    /**
      * after index=2 = before index=3
      *
      * abcd
      *   ||
      *   23
      *
      * @return
      */
    override def beforeThisIndex: Int = index + 1

    override def toString: String = s"after $index"
  }

  def apply(str: String, morphemeProperty: MorphemeProperty = StemAndSuffix): Word = new Word(Seq(Morpheme(str, morphemeProperty)))
}