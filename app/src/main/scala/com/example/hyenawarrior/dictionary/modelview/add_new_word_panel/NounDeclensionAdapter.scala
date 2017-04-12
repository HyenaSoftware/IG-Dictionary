package com.example.hyenawarrior.dictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.example.hyenawarrior.dictionary.modelview.CustomAdapter
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivity
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum

/**
	* Created by HyenaWarrior on 2017.04.12..
	*/
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

		AddNewWordActivity.INDEFINITE_NOUN_EDIT_TEXTS.foreach
		{
			case (id, cs, num) =>
				val tvNC = view.findViewById(id).asInstanceOf[TextView]
				val ncTextForm = map.get(num, cs).getOrElse("...")
				tvNC.setText(ncTextForm)
		}

		view
	}
}
