package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.WordTransformation
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number, Root, Word}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
trait NounStemClass
{
	// (Root, Number, Case) -> Word
	def apply(root: Root, meaningId: Int, decl: (Number, Case)): Word =
	{
		val str = inflect(root, decl)
		val nn = new Noun(str, meaningId, decl, Some(root))
		Word(nn, transformationsFor(decl))
	}

	def transformationsFor(decl: (Number, Case)): List[WordTransformation] = List()

	protected def inflect(root: Root, decl: (Number, Case)): String = root.word + inflection(decl)

	protected def inflection(decl: (Number, Case)): String

	// NounStemClass(valueOfReturn) = (declForm, decl)
	// (String, Number, Case) -> Root
	def unapply(declForm: String, decl: (Number, Case)): Option[Root] =
	{
		val suffix = inflection(decl)

		if(declForm.endsWith(suffix))

			Some(Root(declForm.stripSuffix(suffix)))

		else
			None
	}
}



