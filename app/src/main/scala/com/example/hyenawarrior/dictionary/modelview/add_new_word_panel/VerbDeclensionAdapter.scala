package com.example.hyenawarrior.dictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.example.hyenawarrior.dictionary.modelview.CustomAdapter
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.AddNewVerbHelper
import com.example.hyenawarrior.myapplication.new_word.AddNewVerbHelper.Declension
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{NonFinitiveVerbType, VerbClassEnum, VerbTenseEnum}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object VerbDeclensionAdapter
{
	val VERB_TEXTVIEWS = List(
		R.id.tv_addword_verb_PresSg1 -> Left(SG_1,			PRESENT),
		R.id.tv_addword_verb_PresSg2 -> Left(SG_2,			PRESENT),
		R.id.tv_addword_verb_PresSg3 -> Left(SG_3_MASC,	PRESENT),

		R.id.tv_addword_verb_PresPl1 -> Left(PL_1,			PRESENT),
		R.id.tv_addword_verb_PresPl2 -> Left(PL_2,			PRESENT),
		R.id.tv_addword_verb_PresPl3 -> Left(PL_3_MASC,	PRESENT),

		R.id.tv_addword_verb_PastSg1 -> Left(SG_1,			PAST),
		R.id.tv_addword_verb_PastSg2 -> Left(SG_2,			PAST),
		R.id.tv_addword_verb_PastSg3 -> Left(SG_3_MASC,	PAST),

		R.id.tv_addword_verb_PastPl1 -> Left(PL_1,			PAST),
		R.id.tv_addword_verb_PastPl2 -> Left(PL_2,			PAST),
		R.id.tv_addword_verb_PastPl3 -> Left(PL_3_MASC,	PAST),

		R.id.tv_addword_verb_Infinitive -> Right(INFINITIVE),
		R.id.tv_addword_verb_PresPart		-> Right(PRESENT_PARTICIPLE),
		R.id.tv_addword_verb_PastPart		-> Right(PAST_PARTICIPLE)
	)
}

class VerbDeclensionAdapter(activity: Activity) extends CustomAdapter[(VerbClassEnum, Map[Declension, String])](activity)
{
	override protected def getNewView(i: Int, viewGroup: ViewGroup): View = {

		val view = inflater.inflate(R.layout.verb_declension, viewGroup, false)

		val (VerbClassEnum(vcName, _), map) = itemAt(i)

		val tv_addword_verb_stemName = view.findViewById(R.id.tv_addword_verb_stemName).asInstanceOf[TextView]
		tv_addword_verb_stemName.setText(vcName)

		for ((tvCtrlId, key) <- VerbDeclensionAdapter.VERB_TEXTVIEWS)
		{
			val tvVerbCtrl = view.findViewById(tvCtrlId).asInstanceOf[TextView]
			val text = map.getOrElse(key, "...")
			tvVerbCtrl.setText(text)
		}

		view
	}
}
