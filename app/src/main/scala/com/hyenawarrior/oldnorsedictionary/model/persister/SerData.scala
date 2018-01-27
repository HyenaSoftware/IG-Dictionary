package com.hyenawarrior.oldnorsedictionary.model.persister

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.hyenawarrior.oldnorsedictionary.model.auxiliary.autoClose

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
trait SerData {

  def typeOf(objId: Int): Option[Int]

  def store(typeId: Int, data: Seq[AnyVal]): Int = {

    val baos = new ByteArrayOutputStream()
    val dos = new DataOutputStream(baos)

    val byteArray = autoClose(dos) { dos =>

      data.foreach {

        case i: Int => dos.writeInt(i)
        case b: Byte => dos.writeByte(b)
        case b: Boolean => dos.writeBoolean(b)
      }

      baos.toByteArray
    }

    store(typeId, byteArray)
  }

  def store(typeId: Int, data: Array[Byte]): Int

  def load(objId: Int, typeId: Int): DataInputStream


}
