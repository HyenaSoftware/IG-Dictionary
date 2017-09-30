package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.view.View
import android.widget.TableRow
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewNullHelper extends AddNewPosHelper
{
	override def activate(): Unit = { }

	override def deactivate(): Unit = { }

	override def onRemoveOverride(tableRow: View): Unit = { }

	override def onStemClassSelected(index: Int): Unit = { }

	override def onTextFormOverride(overridingView: View)(str: String): Unit = { }

	override def createOverrideFormSetter(isPrimary: Boolean): View = ???

	override def primaryFromSetter(): View = ???
}
