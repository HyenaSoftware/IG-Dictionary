package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.Button
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.{CustomAdapter, setDeclensionsTo}

/**
	* Created by HyenaWarrior on 2017.04.12..
	*/
object NounDeclensionAdapter
{
	val NOUN_EDIT_TEXTS = List(
		(R.id.tvNewWord_Nom_Sg, 	(SINGULAR, NOMINATIVE))
		, (R.id.tvNewWord_Acc_Sg, (SINGULAR, ACCUSATIVE))
		, (R.id.tvNewWord_Dat_Sg, (SINGULAR, DATIVE))
		, (R.id.tvNewWord_Gen_Sg, (SINGULAR, GENITIVE))

		, (R.id.tvNewWord_Nom_Pl, (PLURAL, NOMINATIVE))
		, (R.id.tvNewWord_Acc_Pl, (PLURAL, ACCUSATIVE))
		, (R.id.tvNewWord_Dat_Pl, (PLURAL, DATIVE))
		, (R.id.tvNewWord_Gen_Pl, (PLURAL, GENITIVE))
	)
}

class NounDeclensionAdapter(activity: Activity, listView: ViewGroup)
  extends CustomAdapter[(NounStemClassEnum, Noun)](activity, listView, R.layout.noun_declension)
{
	private var showDefiniteForms = false

	protected def resetView(i: Int, view: View): Unit = {

		val (nscEnum, noun) = itemAt(i)

		setDeclensionsTo(noun, nscEnum, view, showDefiniteForms)

    // tag the select button
    val tv_addword_noun_Select = view.findViewById[Button](R.id.tv_addword_noun_Select)
    tv_addword_noun_Select.setTag(nscEnum)
	}

	def setDefinitness(isDefinite: Boolean): Unit = {

		showDefiniteForms = isDefinite

		for(i <- 0 until getCount) {

			val v = getView(i, null, null)

			resetView(i, v)
		}
	}

  def getSelectorTagOf(view: View): Option[NounStemClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[NounStemClassEnum])
    case _ => None
  }
}
