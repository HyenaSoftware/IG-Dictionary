package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.{BaseAdapter, ListView}

import scala.collection.mutable

/**
	* Created by HyenaWarrior on 2017.03.10..
	*/
abstract class CustomAdapter[T](val activity: Activity, listView: ViewGroup, layoutElem: Int) extends BaseAdapter
{
	//
	protected val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

	private var items: List[T] = List()

	private var viewCache: mutable.ArraySeq[View] = mutable.ArraySeq()

	//
	listView match {

		case lv: ListView => lv setAdapter this
		case _ => ()
	}

	//
	protected def itemAt(i: Int) = items(i)

	def resetItems(items: List[T]): Unit = {

		this.items = items

    viewCache = new mutable.ArraySeq(items.size)

		listView match {

			case lv: ListView => lv.invalidateViews()
			case _ =>

				listView.removeAllViews()

				items.zipWithIndex
					.map { case(e, i) => getView(i, null, listView) }
					.foreach(v => listView addView v)

				Range(0, getCount)
					.map(i => getView(i, null, listView))
					.foreach(v => listView addView v)
		}
	}

	def add(value: T): Unit = {

		viewCache = viewCache :+ null
		items = items :+ value

		listView match {

			// TODO: it needs to be tested
			case lv: ListView => lv.invalidateViews()

			case _ =>
				val nextIndex = items.size - 1
				listView addView getView(nextIndex, null, listView)
		}
	}

  @Deprecated
	def remove(i: Int): Unit =
	{
		val indiciesToKeep = items.indices.filter(_ != i)
		items = indiciesToKeep.map(items(_)).toList

		val oldViewCache = viewCache
		viewCache = new mutable.ArraySeq(items.size)

		for((v, j) <- indiciesToKeep.map(i => oldViewCache(i)).zipWithIndex) {

			viewCache(j) = v
		}

		listView.removeView(oldViewCache(i))

		notifyDataSetChanged()
	}

	def set(i: Int, value: T): Unit = {

		items = items.zipWithIndex.map {

			case (_, j) if i == j => value -> j
			case e => e
		}
		  .map(_._1)
	}

	private final def getNewView(i: Int, viewGroup: ViewGroup): View = {

		val view = inflater.inflate(layoutElem, viewGroup, false)

		resetView(i, view)

		view
	}

	protected def resetView(i: Int, view: View): Unit

	def getItemId(i: Int): Long = i

  def getItem(i: Int): AnyRef = Long.box(i)

	def getCount: Int = items.size

	def getView(i: Int, prevView: View, viewGroup: ViewGroup): View = {

    var res = viewCache(i)

    if(res == null) {

        res = getNewView(i, viewGroup)
        viewCache(i) = res
    }

    res
	}
}
