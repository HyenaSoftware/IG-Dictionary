package com.example.hyenawarrior.myapplication.new_word
import android.view.View
import android.widget.TableRow
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewNullHelper extends AddNewPosHelper
{
	override def activate(): Unit = { }

	override def deactivate(): Unit = { }

	override def onRemoveOverride(view: TableRow): Unit = { }

	override def onPrimaryTextChange(str: String): Unit = { }

	override def onStemClassSelected(index: Int): Unit = { }

	override def onTextFormOverride(overridingView: View)(str: String): Unit = { }

	override def onNounDeclensionSelected(overridingView: View)(item: (Number, Case)): Unit = { }

	override def onDeclensionSelected(index: Int): Unit = { }
}
