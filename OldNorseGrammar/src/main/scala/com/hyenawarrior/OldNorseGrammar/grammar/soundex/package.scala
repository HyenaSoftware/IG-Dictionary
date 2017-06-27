package com.hyenawarrior.OldNorseGrammar.grammar

/**
	* Created by HyenaWarrior on 2017.06.21..
	*/
package object soundex
{
	val SUFFIX_PRESENT_PARTICIPLE = "(.+)andi".r

	val SUFFIX_ARTICLE_NOM_SG = "(.+)rinn".r
	val SUFFIX_ARTICLE_ACC_SG = "(.+)inn".r
	val SUFFIX_ARTICLE_DAT_SG = "(.+)inum".r
	val SUFFIX_ARTICLE_GEN_SG = "(.+)sins".r

	val SUFFIX_ARTICLE_NOM_PL = "(.+)arnir".r
	val SUFFIX_ARTICLE_ACC_PL = "(.+)ana".r
	val SUFFIX_ARTICLE_DAT_PL = "(.+)unum".r
	val SUFFIX_ARTICLE_GEN_PL = "(.+)anna".r

	// feminine articles
	val SUFFIX_ARTICLE_FEM_NOM_SG = "(.+)r?in".r
	val SUFFIX_ARTICLE_FEM_ACC_SG = "(.+)ina".r
	val SUFFIX_ARTICLE_FEM_DAT_SG = "(.*[á])nni".r
	val SUFFIX_ARTICLE_FEM_GEN_SG = "(.+)rinnar".r

	val SUFFIX_ARTICLE_FEM_NOM_PL = "(.+)rnar".r
	val SUFFIX_ARTICLE_FEM_ACC_PL = "(.+)rnar".r
	val SUFFIX_ARTICLE_FEM_DAT_PL = "(.+)n?um".r
	val SUFFIX_ARTICLE_FEM_GEN_PL = "(.*á)nna".r


	// weak preterite stem
	val SUFFIX_WK_PAST_SG1		= "(.+)[ðdt]a".r
	val SUFFIX_WK_PAST_SG23		= "(.+)[ðdt]ir?".r
	val SUFFIX_WK_PAST_PL			= "(.+)[ðdt]u[ðm]?".r

	val SUFFIX_WK_SUBJ_PL = "(.+)[ðdt]i[mð]?".r

	val SUFFIX_LABIAL_UM = "(.*[á])m".r
	val SUFFIX_UM = "(.+)um".r

	val SUFFIX_NOUN_NOM_PL = "(.+)ar".r
	val SUFFIX_IR = "(.+)ir".r	// preterite subj
	val SUFFIX_IM = "(.+)im".r
	val SUFFIX_IÐ = "(.*[^e])ið".r
	val SUFFIX_UÐ = "(.+)uð".r
	val SUFFIX_ST = "(.+)st".r


	val SUFFIX_NOUN_NOM_SG = "(.+)r".r
	val SUFFIX_NOUN_DAT_SG = "(.+)i".r
	val SUFFIX_NOUN_GEN_SG = "(.+)s".r

	val SUFFIX_NOUN_ACC_GEN_PL = "(.+)a".r
	val SUFFIX_U = "(.+)u".r

	def soundexCodeOf(str: String): String = transformConsonant(removeVowels(addStartingV(removeSuffix(str))))

	private def removeSuffix(str: String) = str match
	{
		case SUFFIX_PRESENT_PARTICIPLE(word) => word

		// articles	+ inflections
		case SUFFIX_ARTICLE_NOM_SG(word) => word
		case SUFFIX_ARTICLE_ACC_SG(word) => word
		case SUFFIX_ARTICLE_DAT_SG(word) => word
		case SUFFIX_ARTICLE_GEN_SG(word) => word

		case SUFFIX_ARTICLE_NOM_PL(word) => word
		case SUFFIX_ARTICLE_ACC_PL(word) => word
		case SUFFIX_ARTICLE_DAT_PL(word) => word
		case SUFFIX_ARTICLE_GEN_PL(word) if str.count(VOWELS contains _) > 2 => word

		// articles	+ inflections for feminine nouns
		case SUFFIX_ARTICLE_FEM_NOM_PL(word) => word
		case SUFFIX_ARTICLE_FEM_ACC_PL(word) => word
		case SUFFIX_ARTICLE_FEM_DAT_PL(word) => word
		case SUFFIX_ARTICLE_FEM_GEN_PL(word) => word

		case SUFFIX_ARTICLE_FEM_NOM_SG(word) => word
		case SUFFIX_ARTICLE_FEM_ACC_SG(word) => word
		case SUFFIX_ARTICLE_FEM_DAT_SG(word) => word
		case SUFFIX_ARTICLE_FEM_GEN_SG(word) => word


		case SUFFIX_WK_PAST_SG1(word) 	if str.count(isConsonant) > 2 => word
		case SUFFIX_WK_PAST_SG23(word)  if str.count(isConsonant) > 2 => word
		case SUFFIX_WK_PAST_PL(word) 		if str.count(isConsonant) > 2 => word
		case SUFFIX_WK_SUBJ_PL(word)  	if str.count(isConsonant) > 2 => word

		// inflections
		case SUFFIX_LABIAL_UM(word) => word
		case SUFFIX_UM(word) => word

		case SUFFIX_NOUN_NOM_PL(word) => word // it has higher priority than SUFFIX_NOUN_NOM_SG
		case SUFFIX_IR(word) => word // it has higher priority than SUFFIX_NOUN_NOM_SG
		case SUFFIX_IM(word) => word
		case SUFFIX_IÐ(word) => word
		case SUFFIX_UÐ(word) => word
		case SUFFIX_ST(word) => word
		case SUFFIX_NOUN_NOM_SG(word) => word
		case SUFFIX_NOUN_DAT_SG(word) => word
		case SUFFIX_NOUN_GEN_SG(word) => word

		case SUFFIX_NOUN_ACC_GEN_PL(word) => word
		case SUFFIX_U(word) => word
		case _ => str
	}

	def isConsonant(c: Char): Boolean =	!(VOWELS contains c)
	def isVowel(c: Char): Boolean =	VOWELS contains c

	private val STARTING_V = "([oóuúyý].+)".r

	private def addStartingV(str: String) = str match
	{
		case STARTING_V(word) => s"v$word"
		case _ => str
	}

	private val VOWELS = "aæáeéiíjoœøóöuúyý" // counts half-vowels too

	private def removeVowels(str: String): String =
	{
		val strTail = str.substring(1).collect
		{
			case c if isConsonant(c) => c
		}

		alterVowel(str.head) + strTail
	}

	private def alterVowel(c: Char): Char = c match
	{
		case 'í' => 'i'
		case 'u' | 'ú' | 'y' | 'ý' | 'ö' => 'a'
		case _ => c
	}

	private val TRANSFORM_NN = "(.*?)n{2,}(.*)".r

	private def transformConsonant(str: String): String = str match
	{
			// as in mann+r => maðr
		case TRANSFORM_NN(pre, post) => s"${pre}ð$post"
		case _ => str
	}
}
