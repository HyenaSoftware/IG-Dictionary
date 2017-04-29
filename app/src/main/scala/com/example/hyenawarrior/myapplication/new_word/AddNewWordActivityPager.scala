package com.example.hyenawarrior.myapplication.new_word

import android.os.Bundle
import android.support.v4.app.{Fragment, FragmentActivity, FragmentStatePagerAdapter}
import android.support.v4.view.ViewPager
import android.support.v7.app.AppCompatActivity
import android.view.View
import com.example.hyenawarrior.myapplication.new_word.pages.{AddNewWordActivity, SetMeaningFragment}
import com.example.hyenawarrior.myapplication.{MainActivity, R}

import scala.math.min

class AddNewWordActivityPager extends AppCompatActivity
{
	outer =>

	lazy val pager = findViewById(R.id.AddNewWordPager).asInstanceOf[ViewPager]

	object AddWordPagerAdapter
		extends FragmentStatePagerAdapter(outer.asInstanceOf[FragmentActivity].getSupportFragmentManager)
	{
		private val pages = List(AddNewWordActivity, SetMeaningFragment)

		private var maxIndex = 0
		private var currentIdx = 0

		def setNewPageIndex(nextIdx: Int): Unit = {

			/*if(nextIdx < currentIdx) {

				maxIndex = nextIdx

				notifyDataSetChanged()
			}*/

			currentIdx = nextIdx
		}

		def allowMoveToTheNextPage(): Int = {

			maxIndex = min(currentIdx + 1, pages.length - 1)

			notifyDataSetChanged()

			maxIndex
		}

		override def getItem(position: Int): Fragment = pages(position)

		override def getCount: Int = maxIndex + 1
	}

	/**
		* Use this listener for
		* - when user swipes back then disable the on-going pages
		*/
	object OnPageListener extends ViewPager.OnPageChangeListener {

		override def onPageScrollStateChanged(state: Int): Unit = { }

		override def onPageScrolled(position: Int, positionOffset: Float, positionOffsetPixels: Int): Unit = { }

		override def onPageSelected(position: Int): Unit = AddWordPagerAdapter.setNewPageIndex(position)
	}

	def selectThisDeclension(view: View): Unit = {

		val nextPageIndex = AddWordPagerAdapter.allowMoveToTheNextPage()

		pager.setCurrentItem(nextPageIndex)
	}

	def saveWordToDatabase(view: View): Unit = finish()

	def addNewOverride(view: View) = AddNewWordActivity.addNewOverride(view)

	def onRemoveOverride(view: View) = AddNewWordActivity.onRemoveOverride(view)

	//
	protected override def onCreate(savedInstanceState: Bundle): Unit = {

		super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_add_new_word_pager)

		pager.setAdapter(AddWordPagerAdapter)
		pager.addOnPageChangeListener(OnPageListener)

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

  }
}
