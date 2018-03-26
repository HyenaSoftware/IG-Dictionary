package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.DATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.PLURAL
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{InvertableTransformation, endsWith, stripSuffix}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
  * Created by HyenaWarrior on 2017.04.09..
  */
trait NounStemClass extends Serializable {

  def associatedGender: Gender

  def thematicVowel: Option[String] = None

  def apply(str: String, declension: NounType, isDefinite: Boolean): Option[String] = {

    val str2 = transformationFor(declension).flatMap(t => t(str)).getOrElse(str)

    val clitic = if(isDefinite) cliticFor(declension) else ""

    val inflectionStr = inflection(declension)

    val correctedInflectionStr = declension match {

      case (PLURAL, DATIVE) if isDefinite => inflectionStr stripSuffix "m"
      case _ => inflectionStr
    }

    Some(str2 + correctedInflectionStr + clitic)
  }

  def unapply(strDeclDef: (String, NounType, Boolean)): Option[String] = {

    val (str, declension, isDefinite) = strDeclDef

    //
    val strCliticRemoved = if(isDefinite) {

      val revClitic = cliticFor(declension)

      val str2 = revClitic.foldRight(str){ case (ce, word) => word stripSuffix ce.toString }

      str2 + (declension match {

        case (PLURAL, DATIVE) => "m"
        case _ => ""
      })

    } else str

    //
    val declSuffix = inflection(declension)

    if(endsWith(strCliticRemoved, declSuffix)) {

      val uninflectedStr = stripSuffix(strCliticRemoved, declSuffix)

      transformationFor(declension)
        .flatMap(t => uninflectedStr match {

          case t(uuStr) => Some(uuStr)
          case _ => None
        })
        .orElse(Some(uninflectedStr))

    } else None
  }

  def transformationFor(decl: NounType): Option[InvertableTransformation] = None

  def inflection(decl: NounType): String

  def cliticFor(decl: NounType): String
}




