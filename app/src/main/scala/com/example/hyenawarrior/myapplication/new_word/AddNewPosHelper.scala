package com.example.hyenawarrior.myapplication.new_word

import android.view.View
import android.widget.TableRow
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
trait AddNewPosHelper
{
	def activate(): Unit
	def deactivate(): Unit
	def onRemoveOverride(view: TableRow)
	def onPrimaryTextChange(str: String): Unit

	def onDeclensionSelected(index: Int): Unit
	def onStemClassSelected(index: Int): Unit
	def onTextFormOverride(overridingView: View)(str: String): Unit
	def onNounDeclensionSelected(overridingView: View)(item: (Number, Case)): Unit
}
