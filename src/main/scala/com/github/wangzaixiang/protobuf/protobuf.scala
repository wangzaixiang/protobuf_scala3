package com.github.wangzaixiang.protobuf

import com.github.wangzaixiang.protobuf.macros.ProtobufSerDerMacros
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import scala.annotation.StaticAnnotation

class tag(seq: Int) extends  StaticAnnotation

trait ProtobufSerDer[T]:
  def encode(instance:T, output: CodedOutputStream): Unit
  def decode(input: CodedInputStream): T

object ProtobufSerDer:
  inline def derived[T]: ProtobufSerDer[T] = ${ ProtobufSerDerMacros.generateProtobufSerderImpl[T] }
  inline given serder[T](using deriving.Mirror.Of[T]): ProtobufSerDer[T] = ${ ProtobufSerDerMacros.generateProtobufSerderImpl[T] }


extension [T : ProtobufSerDer](instance: T)
  def toBytes: Array[Byte] =
    val baos = new java.io.ByteArrayOutputStream(64)
    val output = CodedOutputStream.newInstance(baos)
    summon[ProtobufSerDer[T]].encode(instance, output)
    output.flush()
    baos.toByteArray

extension (bytes: Array[Byte])
  def toBean[T : ProtobufSerDer]: T =
    val input = CodedInputStream.newInstance(bytes)
    summon[ProtobufSerDer[T]].decode(input)