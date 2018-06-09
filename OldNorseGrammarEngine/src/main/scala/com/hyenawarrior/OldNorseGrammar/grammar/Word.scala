package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut

import scala.language.postfixOps

/**
  * Created by HyenaWarrior on 2017.03.01..
  */
case class Word(pos: PoSForm) {

  private val DEFAULT_TRANSFORMATIONS = List(U_Umlaut)
  private val POS_DEPENDENT_TRANSFORMATIONS = pos.transformations

  def underlyingPoS: PoSForm = pos

  // useful for lookup
  def strForm(): String =
  {
    val Syllables(syllables) = pos.strRepr

    val allTransformations = POS_DEPENDENT_TRANSFORMATIONS ++ DEFAULT_TRANSFORMATIONS
    val transformedSyllables = allTransformations.foldLeft(syllables) {

      case (sys, trn) if trn canTransform sys => trn(sys).get
      case (sys, _) => sys
    }

    val str = Syllables(transformedSyllables)

    str

    //pos.strForm
  }

  // formatted description
  val description = "[not yet]"

  override def toString = s"${strForm()} [$pos + ${POS_DEPENDENT_TRANSFORMATIONS.map(_.toString)}]"
}
