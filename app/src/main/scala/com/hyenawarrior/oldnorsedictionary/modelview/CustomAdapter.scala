package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.BaseAdapter

import scala.collection.mutable

/**
	* Created by HyenaWarrior on 2017.03.10..
	*/
abstract class CustomAdapter[T](val activity: Activity) extends BaseAdapter
{
	protected val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

	private var items: List[T] = List()

	private var viewCache: mutable.ArraySeq[View] = mutable.ArraySeq()

	protected def itemAt(i: Int) = items(i)

	def resetItems(items: List[T]): Unit = {

		this.items = items

    viewCache = new mutable.ArraySeq(items.size)
	}

  @deprecated
	def remove(i: Int): Unit =
	{
		items = items.indices.filterNot(_ == i).map(items(_)).toList

		notifyDataSetChanged()
	}

	protected def getNewView(i: Int, viewGroup: ViewGroup): View

	def getItemId(i: Int): Long = i

  def getItem(i: Int): AnyRef = Long.box(i)

	def getCount: Int = items.size

	def getView(i: Int, prevView: View, viewGroup: ViewGroup) = {

    var res = viewCache(i)

    if(res == null) {

        res = getNewView(i, viewGroup)
        viewCache(i) = res
    }

    res
	}
}
