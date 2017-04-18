package com.example.hyenawarrior.dictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.example.hyenawarrior.dictionary.modelview.CustomAdapter
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.12..
	*/
object NounDeclensionAdapter
{
	val NOUN_EDIT_TEXTS = List(
		(R.id.tvNewWord_Nom_Sg, Case.NOMINATIVE,	Number.SINGULAR)
		, (R.id.tvNewWord_Acc_Sg, Case.ACCUSATIVE,	Number.SINGULAR)
		, (R.id.tvNewWord_Dat_Sg, Case.DATIVE,			Number.SINGULAR)
		, (R.id.tvNewWord_Gen_Sg, Case.GENITIVE,		Number.SINGULAR)

		, (R.id.tvNewWord_Nom_Pl, Case.NOMINATIVE,	Number.PLURAL)
		, (R.id.tvNewWord_Acc_Pl, Case.ACCUSATIVE,	Number.PLURAL)
		, (R.id.tvNewWord_Dat_Pl, Case.DATIVE,			Number.PLURAL)
		, (R.id.tvNewWord_Gen_Pl, Case.GENITIVE,		Number.PLURAL)
	)
}

class NounDeclensionAdapter(activity: Activity) extends CustomAdapter[(NounStemClassEnum, Map[(Number, Case), String])](activity)
{
	override protected def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val isSingleList = getCount == 1

		val view = inflater.inflate(R.layout.noun_declension, viewGroup, false)

		val (NounStemClassEnum(ncName, _), map) = itemAt(i)

		//
		val tvNounDeclDesc = view.findViewById(R.id.tvNounDeclDesc).asInstanceOf[TextView]
		tvNounDeclDesc.setText(if (isSingleList)	"" else ncName)

		NounDeclensionAdapter.NOUN_EDIT_TEXTS.foreach
		{
			case (id, cs, num) =>
				val tvNC = view.findViewById(id).asInstanceOf[TextView]
				val ncTextForm = map.get(num, cs).getOrElse("...")
				tvNC.setText(ncTextForm)
		}

		view
	}
}
