package com.example.hyenawarrior.myapplication.new_word.new_pos_helpers

import android.view.View
import android.widget.TableRow
import com.example.hyenawarrior.myapplication.new_word.pages.WordData
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewPosHelper
{
	val DECLENSIONS: Vector[(GNumber, Case)] = GNumber.conventionalValues.flatMap(n => Case.values.map(cs => (n, cs))).toVector
}

trait AddNewPosHelper
{
	def activate(): Unit
	def deactivate(): Unit
	def onRemoveOverride(view: TableRow)
	def onPrimaryTextChange(str: String): Unit

	def onDeclensionSelected(index: Int): Unit
	def onStemClassSelected(index: Int): Unit
	def onTextFormOverride(overridingView: View)(str: String): Unit
	def onNounDeclensionSelected(overridingView: View)(item: (GNumber, Case)): Unit

  def getWordFormsBy(view: View): WordData = null
}
