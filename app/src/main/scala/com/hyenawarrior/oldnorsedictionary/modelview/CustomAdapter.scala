package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.{BaseAdapter, ListView}

/**
	* Created by HyenaWarrior on 2017.03.10..
	*/
abstract class CustomAdapter[T](val activity: Activity, listView: ViewGroup, layoutElem: Int) extends BaseAdapter
{
	//
	protected val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

	private var views:  			List[View]			= List()
	private var values: 			List[T]					= List()
	private var viewToIndex:	Map[View, Int]	= Map()

	//
	listView match {

		case lv: ListView => lv setAdapter this
		case _ => ()
	}

	//
	protected def itemAt(i: Int): T = values(i)

	def allValues = values

	def indexOf(view: View): Int = viewToIndex(view)

	def resetItems(items: List[T]): Unit = {

		// generate the views
		values = items
		views = values.zipWithIndex.map { case (v, i) => getNewView(i, v, listView) }
		viewToIndex = views.zipWithIndex.toMap

		//
		listView match {

			case lv: ListView => lv.invalidateViews()
			case _ =>

				listView.removeAllViews()

				views.foreach(listView.addView)
		}
	}

	def invalidateValues(): Unit = {

		for(((view, value), i) <- (views zip values).zipWithIndex) {

			resetView(i, value, view)
		}
	}

	def add(value: T): Unit = {

		val nextIndex = values.size

		val view = getNewView(nextIndex, value, listView)

		values = values :+ value
		views = views :+ view
		viewToIndex = viewToIndex + (view -> nextIndex)

		listView match {

			// TODO: it needs to be tested
			case lv: ListView => lv.invalidateViews()
			case _ =>	listView addView view
		}
	}

	def remove(index: Int): Unit = {

		val (viewsBefore, viewsAfter) = views splitAt index
		val (valuesBefore, valuesAfter) = values splitAt index

		val viewToRemove = viewsAfter.head

		// update the UI
		listView removeView viewToRemove

		viewToIndex = viewToIndex -- viewsAfter
		viewToIndex = viewToIndex ++ viewsAfter.tail.zipWithIndex.map {

			case (v, i) => v -> (index + i)
		}

		views = viewsBefore ++ viewsAfter.tail
		values = valuesBefore ++ valuesAfter.tail

		for(((value, view), i) <- valuesAfter.tail.zip(viewsAfter.tail).zipWithIndex) {

			indexUpdated(i + index, value, view)
		}
	}

	def remove(view: View): Unit = remove(viewToIndex(view))

	def set(view: View, value: T): Unit = set(viewToIndex(view), value)

	def set(i: Int, value: T): Unit = values = values.zipWithIndex.map {

		case (v, j) if i == j => value
		case (e, _) => e
	}

	private final def getNewView(i: Int, value: T, viewGroup: ViewGroup): View = {

		val view = inflater.inflate(layoutElemFor(value), viewGroup, false)

		initView(view)
		resetView(i, value, view)

		view
	}

	protected def layoutElemFor(value: T): Int = layoutElem

	protected def initView(view: View): Unit = ()
	protected def indexUpdated(i: Int, value: T, view: View): Unit = ()
	protected def resetView(i: Int, value: T, view: View): Unit

	def getItemId(i: Int): Long = i

  def getItem(i: Int): AnyRef = Long.box(i)

	def getCount: Int = values.size

	def getView(i: Int, prevView: View, viewGroup: ViewGroup): View = views(i)
}

