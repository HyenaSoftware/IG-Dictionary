package com.hyenawarrior.oldnorsedictionary

import com.hyenawarrior.oldnorsedictionary.model.database.{TestHashCode, TestNounSerializer, TestPersister, TestStrongVerbContextSerializer}
import org.junit.runner.RunWith
import org.junit.runners.Suite

/**
  * Created by HyenaWarrior on 2017.12.07..
  */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
  classOf[TestPersister],
  classOf[TestStrongVerbContextSerializer],
  classOf[TestNounSerializer],
  classOf[TestSerializable],
  classOf[TestHashCode]
))
class TestSuite
