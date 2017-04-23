package com.example.hyenawarrior.dictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.example.hyenawarrior.dictionary.modelview.CustomAdapter
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbClassEnum, VerbTenseEnum}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object VerbDeclensionAdapter
{
	val VERB_TEXTVIEWS = List(
		(R.id.tv_addword_verb_PresSg1, Pronoun.SG_1,			VerbTenseEnum.PRESENT),
		(R.id.tv_addword_verb_PresSg2, Pronoun.SG_2,			VerbTenseEnum.PRESENT),
		(R.id.tv_addword_verb_PresSg3, Pronoun.SG_3_MASC,	VerbTenseEnum.PRESENT),

		(R.id.tv_addword_verb_PresPl1, Pronoun.PL_1,			VerbTenseEnum.PRESENT),
		(R.id.tv_addword_verb_PresPl2, Pronoun.PL_2,			VerbTenseEnum.PRESENT),
		(R.id.tv_addword_verb_PresPl3, Pronoun.PL_3_MASC,	VerbTenseEnum.PRESENT),

		(R.id.tv_addword_verb_PastSg1, Pronoun.SG_1,			VerbTenseEnum.PAST),
		(R.id.tv_addword_verb_PastSg2, Pronoun.SG_2,			VerbTenseEnum.PAST),
		(R.id.tv_addword_verb_PastSg3, Pronoun.SG_3_MASC,	VerbTenseEnum.PAST),

		(R.id.tv_addword_verb_PastPl1, Pronoun.PL_1,			VerbTenseEnum.PAST),
		(R.id.tv_addword_verb_PastPl2, Pronoun.PL_2,			VerbTenseEnum.PAST),
		(R.id.tv_addword_verb_PastPl3, Pronoun.PL_3_MASC,	VerbTenseEnum.PAST)
	)
}

class VerbDeclensionAdapter(activity: Activity) extends CustomAdapter[(VerbClassEnum, Map[(Pronoun, VerbTenseEnum), String])](activity)
{
	override protected def getNewView(i: Int, viewGroup: ViewGroup): View = {

		val isSingleList = getCount == 1

		val view = inflater.inflate(R.layout.verb_declension, viewGroup, false)

		val (VerbClassEnum(vcName, _), map) = itemAt(i)

		val tv_addword_verb_stemName = view.findViewById(R.id.tv_addword_verb_stemName).asInstanceOf[TextView]
		tv_addword_verb_stemName.setText(if (isSingleList)	"" else vcName)

		VerbDeclensionAdapter.VERB_TEXTVIEWS.foreach
		{
			case (tvCtrlId, pronoun, tense) =>
				val tvVerbCtrl = view.findViewById(tvCtrlId).asInstanceOf[TextView]
				val text = map.getOrElse(pronoun -> tense, "...")
				tvVerbCtrl.setText(text)
		}

		view
	}
}
