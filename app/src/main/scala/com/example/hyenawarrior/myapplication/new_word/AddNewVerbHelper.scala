package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.view.View
import android.widget.{ArrayAdapter, LinearLayout, Spinner, TableRow}
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.{VerbStemClass, VerbStemClassEnum}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
class AddNewVerbHelper(activity: Activity, stemClassSpinner: Spinner) extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.verb_types)
{
	val LL_DECL_LIST = activity.findViewById(R.id.llVerbDeclensions).asInstanceOf[LinearLayout]

	override def activate(): Unit = {

		super.activate()

		LL_DECL_LIST.setVisibility(View.VISIBLE)
	}

	override def deactivate(): Unit = {

		super.activate()

		LL_DECL_LIST.setVisibility(View.GONE)
	}

	override def onStemClassSelected(index: Int): Unit = {

	}

	val LOAD_STEM_CLASS_ENUMS = activity.getResources
		.getStringArray(R.array.verb_types)
		.map
		{
			case "Undefined" => List()
			case "Strong" => List(STRONG_1ST_CLASS, STRONG_2ND_CLASS, STRONG_3RD_CLASS, STRONG_4TH_CLASS, STRONG_5TH_CLASS, STRONG_6TH_CLASS, STRONG_7TH_CLASS)
			case "Weak" => List(WEAK_A_STEM, WEAK_I_STEM, WEAK_J_STEM)
			case str => VerbStemClassEnum.findByName(str).toList
		}
		.toVector

	override def onRemoveOverride(view: TableRow): Unit = {

	}

	override def onPrimaryTextChange(str: String): Unit = {

	}

	override def onDeclensionSelected(index: Int): Unit = {

	}

	override def onTextFormOverride(overridingView: View)(str: String): Unit = {

	}

	override def onNounDeclensionSelected(overridingView: View)(item: (Number, Case)): Unit = {

	}
}
