package com.example.hyenawarrior.dictionary.modelview

import android.text.{Editable, TextWatcher}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
class EditTextTypeListener(callback: String => Unit) extends TextWatcher
{
	override def beforeTextChanged(charSequence: CharSequence, i: Int, i1: Int, i2: Int) = ()

	override def onTextChanged(charSequence: CharSequence, i: Int, i1: Int, i2: Int): Unit =
	{
		val str: String = charSequence.toString

		callback(str)
	}

	override def afterTextChanged(editable: Editable) = ()
}
