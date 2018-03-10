package com.hyenawarrior.oldnorsedictionary

import android.support.test.InstrumentationRegistry
import android.support.test.filters.SmallTest
import android.support.test.runner.AndroidJUnit4
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith

/**
  * Created by HyenaWarrior on 2018.03.10..
  *
  * developer.android.com/training/testing/unit-testing/instrumented-unit-tests.html
  */
@RunWith(classOf[AndroidJUnit4])
@SmallTest
class SampleTest {

  @Test
  def test1(): Unit = {

    val context = InstrumentationRegistry.getContext

    val packageName = context.getPackageName

    assertNotNull(packageName)
  }

}
