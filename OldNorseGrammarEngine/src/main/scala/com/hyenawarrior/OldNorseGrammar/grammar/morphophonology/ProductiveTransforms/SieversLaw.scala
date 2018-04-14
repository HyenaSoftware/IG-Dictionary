package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length._
import com.hyenawarrior.OldNorseGrammar.grammar.Syllables
import com.hyenawarrior.auxiliary.&

/**
  * Created by HyenaWarrior on 2018.03.18..
  */
object SieversLaw {

    /*
      * Syncope?
      * rí-kij -> rí-ki
      * rí-ki -> rí-kij
      * rík-jum -> rí-ki-jum
      *
      * rí-ki
      * ríkj-
      */
  private val svJ = ".+[^y]j.*".r

  def restore(str: String): Option[String] = str match {

    case Syllables(firstSy :: _) & svJ() if firstSy.length == OVERLONG => Some { str.replace("j", "ij") }

    case Syllables(firstSy :: _) if firstSy.length == LONG && str.endsWith("i") => Some { str.replace("i", "ij") }

    case _ => None
  }
}
