package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.WordTransformation
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar._

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
trait NounStemClass
{
	// (Root, Number, Case) -> Word
	def apply(root: Root, meaningId: Int, decl: (GNumber, Case)): Word =
	{
		val str = inflect(root, decl)
		val nn = new Noun(str, meaningId, decl, root, this)

		Word(nn)
	}

	def transformationsFor(decl: (GNumber, Case)): List[WordTransformation] = List()

	protected def inflect(root: Root, decl: (GNumber, Case)): String = root.word + inflection(decl)

	protected def inflection(decl: (GNumber, Case)): String

	// NounStemClass(valueOfReturn) = (declForm, decl)
	// (String, Number, Case) -> Root
	def unapply(declForm: String, decl: (GNumber, Case)): Option[Root] =
	{
		val suffix = inflection(decl)

		if(declForm.endsWith(suffix))

			Some(Root(declForm.stripSuffix(suffix)))

		else
			None
	}
}




