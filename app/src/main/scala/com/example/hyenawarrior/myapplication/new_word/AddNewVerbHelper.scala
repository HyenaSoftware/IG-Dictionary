package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.widget.{ArrayAdapter, Spinner}
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.{VerbStemClass, VerbStemClassEnum}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
class AddNewVerbHelper(activity: Activity) extends AbstractAddNewPosHelper[VerbStemClassEnum](activity, R.array.verb_types)
{
	override protected def onStemClassSelected(newStemClassList: List[VerbStemClassEnum]): Unit = {

	}

	override protected def loadStemClassEnums: Vector[List[VerbStemClassEnum]] = activity.getResources
		.getStringArray(R.array.verb_types)
		.map
		{
			case "Undefined" => List()
			case "Strong" => List(STRONG_1ST_CLASS, STRONG_2ND_CLASS, STRONG_3RD_CLASS, STRONG_4TH_CLASS, STRONG_5TH_CLASS, STRONG_6TH_CLASS, STRONG_7TH_CLASS)
			case "Weak" => List(WEAK_A_STEM, WEAK_I_STEM, WEAK_J_STEM)
			case str => VerbStemClassEnum.findByName(str).toList
		}
		.toVector
}
