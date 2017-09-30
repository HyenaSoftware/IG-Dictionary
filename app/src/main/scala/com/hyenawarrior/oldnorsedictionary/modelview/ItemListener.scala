package com.hyenawarrior.oldnorsedictionary.modelview

import android.view.View
import android.widget.AdapterView
import android.widget.AdapterView.OnItemSelectedListener

/**
	* Created by HyenaWarrior on 2017.06.18..
	*/
class ItemListener(callback: Int => Unit) extends OnItemSelectedListener
{
	override def onNothingSelected(adapterView: AdapterView[_]) = ()

	override def onItemSelected(adapterView: AdapterView[_], view: View, index: Int, l: Long): Unit =
	{
		callback(index)
	}
}