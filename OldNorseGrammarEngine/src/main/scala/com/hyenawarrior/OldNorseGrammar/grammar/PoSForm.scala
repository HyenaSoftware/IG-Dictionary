package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{U_Umlaut, WordTransformation}

/**
  * Created by HyenaWarrior on 2017.03.20..
  *
  * Part of Speech - Szófaj
  */
trait PoSForm extends Serializable {

  def strRepr: String

  def transformations: List[WordTransformation] = List(U_Umlaut)
}
