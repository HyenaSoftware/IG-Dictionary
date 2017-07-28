package com.example.hyenawarrior.dictionary.modelview

import android.view.View
import android.view.View.OnClickListener

/**
	* Created by HyenaWarrior on 2017.07.24..
	*/
case class ClickListener(callback: () => Unit) extends OnClickListener
{
	override def onClick(view: View): Unit = callback()
}
