package com.example.hyenawarrior.myapplication.new_word

import android.view.View
import android.widget.AdapterView
import android.widget.AdapterView.OnItemSelectedListener

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
class SpinnerListener[T](entries: List[T], callback: T => Unit) extends OnItemSelectedListener
{
	override def onNothingSelected(adapterView: AdapterView[_]) = ()

	override def onItemSelected(adapterView: AdapterView[_], view: View, i: Int, l: Long): Unit =
	{
		val item = entries(i)

		callback(item)
	}
}
