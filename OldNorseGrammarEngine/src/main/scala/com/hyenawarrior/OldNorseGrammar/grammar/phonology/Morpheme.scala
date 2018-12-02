package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Syncopated

import scala.collection.mutable

/**
  * Created by HyenaWarrior on 2018.09.21..
  *
  * https://en.wikipedia.org/wiki/Morpheme#Other_features_of_morphemes
  *
  * TODO: how morphemes and phonemes correlate to syllables?
  * should we have two parallel model of the words?
  */

trait Morpheme {

  def lengthInLetters: Int

  def asString(): String
}

object Morpheme {

  def apply(str: String, morphemeProperty: MorphemeProperty): SimpleMorpheme = {

    val normalizedStr = normalize(str)

    var i = 0

    val lb = new mutable.ListBuffer[Phoneme]

    while(i < normalizedStr.length) {

      val ss = normalizedStr substring i

      val (ph, l) = Phoneme extract ss

      i += l

      lb += ph
    }

    SimpleMorpheme(lb, morphemeProperty)
  }

  /**
    * resolve digraphs
    * stabilize vowels
    *
    * @param str
    * @return
    */
  private def normalize(str: String): String = str
    .replace("ae", "æ")
    .replace("oe", "œ")
    .replace("ö", "ǫ")
    .replace("ts", "z")
    .replace("ks", "x")
    //
    .replace("z", "z1z2")
    .replace("x", "x1x2")
}

final case class SimpleMorpheme(phonemes: Seq[Phoneme], morphemeProperty: MorphemeProperty) extends Morpheme {

  def lengthInLetters: Int = phonemes.map(_.lengthInLetters).sum

  def splitAtToMorphemes(location: Int): Option[(SimpleMorpheme, SimpleMorpheme)] = splitAtTo(location) match {

    case Some((seq1, seq2)) =>
      val sm1 = SimpleMorpheme(seq1, morphemeProperty)
      val sm2 = SimpleMorpheme(seq2, morphemeProperty)

      Some(sm1 -> sm2)

    case None => None
  }

  def splitAtTo(location: Int): Option[(Seq[Phoneme], Seq[Phoneme])] = {

    var phonemeIndex = -1
    var splitLoc = location + (if(location < 0) lengthInLetters else 0)

    while(splitLoc > 0 && phonemeIndex + 1 < phonemes.length) {

      phonemeIndex += 1

      splitLoc = splitLoc - phonemes(phonemeIndex).lengthInLetters
    }

    if(-2 < phonemeIndex && phonemeIndex < phonemes.length) {

      // API split works a bit differently, so +1
      Some(phonemes.splitAt(phonemeIndex + 1))

    } else { None }
  }

  def +(ph: Phoneme): SimpleMorpheme = SimpleMorpheme(phonemes :+ ph, morphemeProperty)

  def is(morphemeProperty: MorphemeProperty): Boolean = this.morphemeProperty == morphemeProperty

  type Categorizer[T] = Phoneme => T
  type PhonemeTransformer = PartialFunction[(Int, Phoneme), Phoneme]

  def transformPhomemes[T](categorizer: Categorizer[T], f: PhonemeTransformer, reverseIndicies: Boolean = false): SimpleMorpheme = {

    val indexedPhonemesWithProperties = phonemes
      // create "global indicies" to have the phonemes in the same order during the process
      //.zipWithIndex.map { case (k, idx) => idx -> k }
      .indices.zip(phonemes)
      .groupBy { case (_, ph) => categorizer(ph) }
      // category type is no longer needed, just the created subgroups, whatever their keys are
      .values
      // create indicies for the phonemes in the subgroups, after that the sub-streams are not needed, so flattening them
      .flatMap {
        e =>
          val rs = if(reverseIndicies) e.reverse else e
          rs.zipWithIndex.map { case ((globalIndex, ph), indexInGroup) => globalIndex -> (indexInGroup, ph) }
      }
      // order the phonemes and drop the global indicies
      .toSeq.sortBy(_._1).map(_._2)

    val newPhonemesWithProperties = indexedPhonemesWithProperties.map {

      case e if f isDefinedAt e => f(e)
      case (_, ph) => ph
    }

    SimpleMorpheme(newPhonemesWithProperties, morphemeProperty)
  }

  def transformPhonemesRight[P <: Phoneme](selector: PartialFunction[Phoneme, P], transformer: PartialFunction[(Int, P), Phoneme]): SimpleMorpheme = {

    val ZERO = (0, List[Phoneme]())

    val (_, newList) = phonemes.foldRight(ZERO){

      case (e, (index, list)) =>

      val (newIndex, newElem) = if(selector.isDefinedAt(e)) {

        val elem = selector(e)

        if(transformer.isDefinedAt(index -> elem)) {

          (index + 1) -> transformer(index -> elem)

        } else { index -> e }

      } else { index -> e }

      val outputList = newElem :: list

      newIndex -> outputList
    }

    SimpleMorpheme(newList, morphemeProperty)
  }

  def transformPhonemes[P <: Phoneme](selector: PartialFunction[Phoneme, P], transformer: PartialFunction[(Int, P), Phoneme]): SimpleMorpheme = {

    val ZERO = (0, List[Phoneme]())

    val (_, newList) = phonemes.foldLeft(ZERO){

      case ((index, list), e) =>

        val (newIndex, newElem) = if(selector.isDefinedAt(e)) {

          val elem = selector(e)

          if(transformer.isDefinedAt(index -> elem)) {

            index + 1 -> transformer(index -> elem)

          } else { index + 1 -> e }

        } else { index -> e }

        val outputList = newElem :: list

        newIndex -> outputList
    }

    SimpleMorpheme(newList.reverse, morphemeProperty)

  }

  def asString(): String = phonemes.collect { case ph if ph.phonemeProperty != Syncopated => ph.asString }.mkString

  def asString(filter: PhonemeProperty => Boolean): String = phonemes.filter(ph => filter(ph.phonemeProperty)).map(_.asString).mkString

  override def toString: String = phonemes.map(ph => ph.asString).mkString
}
