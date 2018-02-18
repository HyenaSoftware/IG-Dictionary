package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.InvertableTransformation
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
trait NounStemClass extends Serializable {

	def thematicVowel: Option[String] = None

	def apply(str: String, declension: NounType): Option[String] = {

		val str2 = transformationFor(declension).flatMap(t => t(str)).getOrElse(str)

		Some(str2 + inflection(declension))
	}

	def unapply(strDecl: (String, NounType)): Option[String] = {

		val (str, declension) = strDecl
		val declSuffix = inflection(declension)

		if(!str.endsWith(declSuffix)) {

			None

		} else {

			val uninflectedStr = str stripSuffix declSuffix

			transformationFor(declension)
				.flatMap(t => uninflectedStr match {

					case t(uuStr) => Some(uuStr)
					case _ => None
				})
				.orElse(Some(uninflectedStr))
		}
	}

	def transformationFor(decl: NounType): Option[InvertableTransformation] = None

	def inflection(decl: NounType): String
}




