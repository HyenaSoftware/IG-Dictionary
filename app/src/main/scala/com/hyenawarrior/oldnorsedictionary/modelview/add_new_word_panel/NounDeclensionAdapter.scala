package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.{Button, TextView}
import com.hyenawarrior.OldNorseGrammar.grammar.Case.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.CustomAdapter

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
  extends CustomAdapter[(NounStemClassEnum, Map[NounType, String])](activity, listView, R.layout.noun_declension)
{
	protected def resetView(i: Int, view: View): Unit = {

		val isSingleList = getCount == 1

		val (nscEnum @ NounStemClassEnum(ncName, _), map) = itemAt(i)

		//
		val tvNounDeclDesc = view.findViewById(R.id.tvNounDeclDesc).asInstanceOf[TextView]
		tvNounDeclDesc.setText(if (isSingleList)	"" else ncName)

		NounDeclensionAdapter.NOUN_EDIT_TEXTS.foreach
		{
			case (id, nf) =>
				val tvNC = view.findViewById(id).asInstanceOf[TextView]
				val ncTextForm = map.getOrElse(nf, "...")
				tvNC.setText(ncTextForm)
		}

    // tag the select button
    val tv_addword_noun_Select = view.findViewById(R.id.tv_addword_noun_Select).asInstanceOf[Button]
    tv_addword_noun_Select.setTag(nscEnum)
	}

  def getSelectorTagOf(view: View): Option[NounStemClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[NounStemClassEnum])
    case _ => None
  }
}
