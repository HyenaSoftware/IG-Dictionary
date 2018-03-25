package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length._
import com.hyenawarrior.OldNorseGrammar.grammar.Syllables

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
  def restore(str: String): Option[String] = str match {

    case Syllables(firstSy :: _) if firstSy.length == OVERLONG && str.contains("j") => Some {

        str.replace("j", "ij")
      }

    case _ => None
  }
}
