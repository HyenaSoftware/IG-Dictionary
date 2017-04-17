package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum._

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
class AddNewNounHelper(activity: Activity) extends AbstractAddNewPosHelper[NounStemClassEnum](activity, R.array.noun_types)
{
	override def onStemClassSelected(newStemClassList: List[NounStemClassEnum]) {

	}

	override def loadStemClassEnums: Vector[List[NounStemClassEnum]] =	activity.getResources
		.getStringArray(R.array.noun_types)
		.map
		{
			case "Undefined" => List()
			case "Feminine" => List(STRONG_FEMININE_A, STRONG_FEMININE_I, STRONG_FEMININE_R, WEAK_FEMININE_I, WEAK_FEMININE_U)
			case "Masculine" => List(STRONG_MASCULINE_A, STRONG_MASCULINE_I, STRONG_MASCULINE_R, STRONG_MASCULINE_U, WEAK_MASCULINE_A, WEAK_MASCULINE_R)
			case "Neuter" => List(STRONG_NEUTER, WEAK_NEUTER_U)
			case str => NounStemClassEnum.findByName[NounStemClassEnum](str).toList
		}
		.toVector
}
