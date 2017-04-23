package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class Ablaut(rootVowel: String)

object AblautTransformation
{
	def apply(str: String, srcAblaut: Ablaut, dstAblaut: Ablaut): Option[String] =
	{
		val where = str.indexOf(srcAblaut.rootVowel)

		if (where != -1)
		{

			val length = srcAblaut.rootVowel.length

			val prefixStr = str.substring(0, where)
			val suffixStr = str.substring(where + length)

			Some(s"$prefixStr${dstAblaut.rootVowel}$suffixStr")

			//val rootVowel = root.rootVowel
			//root.word.replace(rootVowel, ablautVowel)
		} else None
	}

/*	def forceApply(syllables: List[Syllable]): List[Syllable] = {

		syllables.headOption
			.map(firstSyll => firstSyll.stressed -> firstSyll.letters)
		  .map{case (isS, str) => Syllable(str, isS)}
			.map(newSyl => newSyl +: syllables.tail)
			.getOrElse(List())
	}
*/

}
