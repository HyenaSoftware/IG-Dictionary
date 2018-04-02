package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{InvertableTransformation, endsWith, stripSuffix}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
  * Created by HyenaWarrior on 2017.04.09..
  */
trait NounStemClass extends Serializable {

  def associatedGender: Gender

  def thematicVowel: Option[String] = None

  def apply(str: String, declension: NounType): Option[String] = {

    val str2 = transformationFor(declension).flatMap(t => t(str)).getOrElse(str)

    val inflectionStr = inflection(declension)

    Some(str2 + inflectionStr)
  }

  def unapply(strDeclDef: (String, NounType)): Option[String] = {

    val (str, declension) = strDeclDef

    //
    val declSuffix = inflection(declension)

    if(endsWith(str, declSuffix)) {

      val uninflectedStr = stripSuffix(str, declSuffix)

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




