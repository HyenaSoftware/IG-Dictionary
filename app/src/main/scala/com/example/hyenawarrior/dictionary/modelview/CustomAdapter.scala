package com.example.hyenawarrior.dictionary.modelview
import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.BaseAdapter

/**
	* Created by HyenaWarrior on 2017.03.10..
	*/
abstract class CustomAdapter[T](val activity: Activity) extends BaseAdapter
{
	protected val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

	private var items: List[T] = List()

	protected def itemAt(i: Int) = items(i)

	def resetItems(items: List[T]): Unit =
	{
		this.items = items
	}

	def remove(i: Int): Unit =
	{
		items = items.indices.filterNot(_ == i).map(items(_)).toList

		notifyDataSetChanged()
	}

	protected def getNewView(i: Int, viewGroup: ViewGroup): View

	def getItemId(i: Int): Long = i

	def getCount: Int = items.size

	def getView(i: Int, prevView: View, viewGroup: ViewGroup) =
	{
		getNewView(i, viewGroup)
	}

	def getItem(i: Int): AnyRef = Long.box(i)

}
