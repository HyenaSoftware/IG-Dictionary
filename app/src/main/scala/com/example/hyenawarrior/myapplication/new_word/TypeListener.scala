package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.text.{Editable, TextWatcher}
import android.widget.TextView
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
class TypeListener(activity: Activity) extends TextWatcher
{
	val INDEFINITE_NOUN_EDIT_TEXTS = List(
		(R.id.tvNewWord_Nom_Sg, Case.NOMINATIVE,	Number.SINGULAR)
		, (R.id.tvNewWord_Acc_Sg, Case.ACCUSATIVE,	Number.SINGULAR)
		, (R.id.tvNewWord_Dat_Sg, Case.DATIVE,			Number.SINGULAR)
		, (R.id.tvNewWord_Gen_Sg, Case.GENITIVE,		Number.SINGULAR)

		, (R.id.tvNewWord_Nom_Pl, Case.NOMINATIVE,	Number.PLURAL)
		, (R.id.tvNewWord_Acc_Pl, Case.ACCUSATIVE,	Number.PLURAL)
		, (R.id.tvNewWord_Dat_Pl, Case.DATIVE,			Number.PLURAL)
		, (R.id.tvNewWord_Gen_Pl, Case.GENITIVE,		Number.PLURAL)
	)

	override def beforeTextChanged(charSequence: CharSequence, i: Int, i1: Int, i2: Int) = ()

	override def onTextChanged(charSequence: CharSequence, i: Int, i1: Int, i2: Int): Unit =
	{
		INDEFINITE_NOUN_EDIT_TEXTS
			.map({ case (id, _, _) => id })
			.map(id => activity.findViewById(id).asInstanceOf[TextView])
			.foreach(v => v.setText(charSequence))
	}

	override def afterTextChanged(editable: Editable) = ()
}
