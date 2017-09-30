package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{FinitiveStrongVerb, NonFinitiveStrongVerb, NonFinitiveVerbType}
import com.hyenawarrior.oldnorsedictionary.R

/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class WordFormAdapter(activity: Activity) extends CustomAdapter[Word](activity)
{
	override def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val view = inflater.inflate(R.layout.word_form_entry, viewGroup)
		//val rootLayout = view.asInstanceOf[GridLayout]

		val item = itemAt(i)

		val tvWordForm = view.findViewById(R.id.tvWordForm).asInstanceOf[TextView]
		val tvWordDesc = view.findViewById(R.id.tvWordDesc).asInstanceOf[TextView]

		tvWordForm setText item.strForm
		//tvWordDesc setText formatTraits(item.traits)

		val formDesc = item.pos match
		{
			case n: Noun => s"[${shortCut(n.decl._1)}, ${shortCut(n.decl._2)}]"
			case v: FinitiveStrongVerb => s"[${shortCut(v.pronoun.number)}, ${v.pronoun.person}]"
			case v: NonFinitiveStrongVerb => s"[${shortCut(v.nonFinitiveVerbType)}]"
			case _ => "???"
		}

		tvWordDesc setText formDesc

		view
	}

	def formatTraits(trs: List[DescriptorFlag]): String = trs.map(t => shortCut(t)).mkString("[", ", ", "]")

	def shortCut(df: Any) = df match
	{
		case Case.NOMINATIVE => "nom"
		case Case.ACCUSATIVE => "acc"
		case Case.DATIVE => "dat"
		case Case.GENITIVE => "gen"
		case GNumber.SINGULAR => "sg"
		case GNumber.PLURAL => "pl"
		case NonFinitiveVerbType.INFINITIVE => "inf"
		case NonFinitiveVerbType.PRESENT_PARTICIPLE => "present part."
		case NonFinitiveVerbType.PAST_PARTICIPLE => "past part."
	}
}
