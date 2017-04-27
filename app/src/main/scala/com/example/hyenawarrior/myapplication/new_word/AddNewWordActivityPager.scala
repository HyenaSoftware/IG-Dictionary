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

	object AddWordPagerAdapter
		extends FragmentStatePagerAdapter(outer.asInstanceOf[FragmentActivity].getSupportFragmentManager)
	{
		private val pages = List(AddNewWordActivity, SetMeaningFragment)

		private var maxItem = 1

		def allowToMoveForward(): Unit = {

			maxItem = maxItem + 1

			notifyDataSetChanged()
		}

		override def getItem(position: Int): Fragment = pages(position)

		override def getCount: Int = min(maxItem, pages.length)
	}

	def selectThisDeclension(view: View): Unit = {
		AddWordPagerAdapter.allowToMoveForward()

		val lastPageIndex = AddWordPagerAdapter.getCount - 1

		val pager = findViewById(R.id.AddNewWordPager).asInstanceOf[ViewPager]
		pager.setCurrentItem(lastPageIndex)
	}

	def addNewOverride(view: View) = AddNewWordActivity.addNewOverride(view)

	def onRemoveOverride(view: View) = AddNewWordActivity.onRemoveOverride(view)

	//
	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_add_new_word_pager)

		val fm = this.asInstanceOf[FragmentActivity]
		val pager = findViewById(R.id.AddNewWordPager).asInstanceOf[ViewPager]
		pager.setAdapter(AddWordPagerAdapter)

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

  }
}
