package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.view.View
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.WordData

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
	def onRemoveOverride(tableRow: View)

	def primaryFromSetter(): View
	def createOverrideFormSetter(isPrimary: Boolean = false) : View

	def onStemClassSelected(index: Int): Unit
	def onTextFormOverride(overridingView: View)(str: String): Unit

  def getWordFormsBy(view: View): WordData = null
}
