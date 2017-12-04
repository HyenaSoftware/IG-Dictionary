package com.hyenawarrior.oldnorsedictionary

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.language.postfixOps

/**
  * Created by HyenaWarrior on 2017.12.04..
  */
object uncaughtExceptionHandler extends Thread.UncaughtExceptionHandler {

  private val defaultEventHandler = Thread getDefaultUncaughtExceptionHandler

  override def uncaughtException(t: Thread, e: Throwable): Unit = try {

    val sdCard = "/storage/extSdCard"

    val sdf = new SimpleDateFormat("yyyyMMMdd_HHmmss", Locale.ENGLISH)

    val now = new Date(System.currentTimeMillis())
    val time = sdf.format(now)

    val dumpFile = new File(sdCard, s"exceptionDump_$time.txt")

    var pw: PrintWriter = null

    try {

      pw = new PrintWriter(new FileWriter(dumpFile))
      e.printStackTrace(pw)

    } finally {

      if(pw != null) pw.close()
    }

  } catch {

    case e: Exception =>
      val msg = e.getMessage
      android.util.Log.w(uncaughtExceptionHandler.getClass.getSimpleName, msg)

  } finally {

    defaultEventHandler.uncaughtException(t, e)
  }
}
