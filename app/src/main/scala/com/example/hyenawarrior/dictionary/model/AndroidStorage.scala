package com.example.hyenawarrior.dictionary.model

import java.io.File

import android.os.Environment

import scala.io.Source

/**
	* Created by HyenaWarrior on 2017.03.22..
	*/
trait AndroidStorage
{
	/* Checks if external storage is available for read and write */
	def isExternalStorageWritable =
	{
		val state = Environment.getExternalStorageState

		Environment.MEDIA_MOUNTED == state
	}

	/* Checks if external storage is available to at least read */
	def isExternalStorageReadable =
	{
		val state = Environment.getExternalStorageState

		(Environment.MEDIA_MOUNTED == state || Environment.MEDIA_MOUNTED_READ_ONLY == state)
	}

	def availableLanguages: List[String] =
	{
		val docDir = Environment getExternalStoragePublicDirectory "Documents"
		docDir.listFiles.map(_.getName).toList
	}

	protected def getEntries(language: String) = Source.fromFile(fileOf(language))

	private def fileOf(language: String): File =
	{
		val docDir = Environment getExternalStoragePublicDirectory "Documents"

		new File(s"$docDir/$language/", "data.txt")
	}
}
