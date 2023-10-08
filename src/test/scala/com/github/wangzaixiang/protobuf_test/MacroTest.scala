package com.github.wangzaixiang.protobuf_test

import org.scalatest.funsuite.AnyFunSuite
import com.github.wangzaixiang.protobuf.*

object MacroTest:

  def check[T: ProtobufSerDer](bean: T): Unit =
    val bytes = bean.toBytes
    val bean2 = bytes.toBean[T]
    assert(bean == bean2)

class MacroTest extends AnyFunSuite {

  test("simple bean") {

    case class SimpleBean
    (
      @tag(1) id: Int,
      @tag(2) count: Int
    ) derives ProtobufSerDer

    val bean = SimpleBean(1, 2)

    MacroTest.check(bean)

  }

  test("more primitive") {
    case class SimpleBean
    (
      @tag(1) id: Int,
      @tag(2) count: Int,
      @tag(3) i64: Long
    ) derives ProtobufSerDer

    val bean = SimpleBean(1, 2, 3L)
    MacroTest.check(bean)

  }

}
