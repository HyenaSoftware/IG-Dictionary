package com.hyenawarrior.oldnorsedictionary.modelview

import android.widget.SearchView
import com.hyenawarrior.OldNorseGrammar.grammar.Database

/**
	* Created by HyenaWarrior on 2016.10.16..
	*/
class TypeListener(db: Database) extends SearchView.OnQueryTextListener
{
	override def onQueryTextSubmit(query: String): Boolean =
	{

		false
	}

	override def onQueryTextChange(newText: String): Boolean =
	{

		false
	}
}
