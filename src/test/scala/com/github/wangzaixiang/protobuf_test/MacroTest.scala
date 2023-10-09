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
      @tag(1) bool: Boolean,
      @tag(2) i32: Int,
      @tag(3) i64: Long,
      @tag(4) f32: Float,
      @tag(5) f64: Double,
      @tag(6) str: String
    ) derives ProtobufSerDer

    val bean = SimpleBean(bool = true, i32 = 10, i64 = 200L, f32 = 12.34f, f64 = 1234.56, str = "Hello")
    MacroTest.check(bean)

  }

  test("repeated primitive"){
    case class SimpleBean
    (
      @tag(1) bool: Seq[Boolean],
      @tag(2) i32: Seq[Int],
      @tag(3) i64: Seq[Long],
      @tag(4) f32: Seq[Float],
      @tag(5) f64: Seq[Double],
      @tag(6) str: Seq[String]
    ) derives ProtobufSerDer

    val bean = SimpleBean(bool = List(true,false), i32 = List(10,20), i64 = List(200L,300L), f32 = List(12.34f, 123.45f),
      f64 = List(1234.56, 2345.67), str = List("Hello", "world") )
    MacroTest.check(bean)
  }

  test("simple reference") {

    case class Bean1
    (
      @tag(1) bool: Boolean,
      @tag(2) bean1: Bean1
    ) derives ProtobufSerDer // TODO the code can't passed compile

    val bean1 = Bean1(true, null)
    MacroTest.check(bean1)
  }

  test("simple reference 2") {

    case class Bean1
    (
      @tag(1) bool: Boolean,
      @tag(2) bean1: Bean1,
      @tag(3) bean2: Bean2
    )derives ProtobufSerDer // TODO the code can't passed compile

    case class Bean2
    (
      @tag(1) i32: Int,
      @tag(2) str: String
    ) derives ProtobufSerDer

    val bean1 = Bean1(true, null, Bean2(10, "Hello"))
    MacroTest.check(bean1)
  }

  ignore("simple reference 3 not supported now") {

    case class Bean1
    (
      @tag(1) bool: Boolean,
      @tag(2) bean1: Bean1,
      @tag(3) bean2: Bean2
    ) // derives ProtobufSerDer // TODO the code can't passed compile

    case class Bean2
    (
      @tag(1) i32: Int,
      @tag(2) str: String
    )

    val bean1 = Bean1(true, null, Bean2(10, "Hello"))
//    MacroTest.check(bean1)
  }

}
