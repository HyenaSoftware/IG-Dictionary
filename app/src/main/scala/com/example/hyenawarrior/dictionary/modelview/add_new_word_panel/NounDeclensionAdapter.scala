package com.example.hyenawarrior.dictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.{Button, TextView}
import com.example.hyenawarrior.dictionary.model.database.marshallers.NounForm
import com.example.hyenawarrior.dictionary.modelview.CustomAdapter
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum

/**
	* Created by HyenaWarrior on 2017.04.12..
	*/
object NounDeclensionAdapter
{
	val NOUN_EDIT_TEXTS = List(
		(R.id.tvNewWord_Nom_Sg, 	NounForm.NOUN_NOM_SG)
		, (R.id.tvNewWord_Acc_Sg, NounForm.NOUN_ACC_SG)
		, (R.id.tvNewWord_Dat_Sg, NounForm.NOUN_DAT_SG)
		, (R.id.tvNewWord_Gen_Sg, NounForm.NOUN_GEN_SG)

		, (R.id.tvNewWord_Nom_Pl, NounForm.NOUN_NOM_PL)
		, (R.id.tvNewWord_Acc_Pl, NounForm.NOUN_ACC_PL)
		, (R.id.tvNewWord_Dat_Pl, NounForm.NOUN_DAT_PL)
		, (R.id.tvNewWord_Gen_Pl, NounForm.NOUN_GEN_PL)
	)
}

class NounDeclensionAdapter(activity: Activity) extends CustomAdapter[(NounStemClassEnum, Map[NounForm, String])](activity)
{
	override protected def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val isSingleList = getCount == 1

		val view = inflater.inflate(R.layout.noun_declension, viewGroup, false)

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

		view
	}

  def getSelectorTagOf(view: View): Option[NounStemClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[NounStemClassEnum])
    case _ => None
  }
}
