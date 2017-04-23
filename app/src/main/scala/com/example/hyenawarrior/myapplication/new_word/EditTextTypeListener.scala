package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.text.{Editable, TextWatcher}
import android.widget.TextView
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber$}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
class EditTextTypeListener(activity: Activity, callback: String => Unit) extends TextWatcher
{
	override def beforeTextChanged(charSequence: CharSequence, i: Int, i1: Int, i2: Int) = ()

	override def onTextChanged(charSequence: CharSequence, i: Int, i1: Int, i2: Int): Unit =
	{
		val str: String = charSequence.toString

		callback(str)
	}

	override def afterTextChanged(editable: Editable) = ()
}
