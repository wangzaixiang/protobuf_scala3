package com.github.wangzaixiang.protobuf.macros

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import scala.quoted.*
import com.github.wangzaixiang.protobuf.*


trait Generator[T: Type]:
  def generate(using q: Quotes)(deps: Map[q.reflect.TypeRepr, q.reflect.Term]): Expr[ProtobufSerDer[T]]
  def sourceTpe(using q: Quotes): q.reflect.TypeRepr = q.reflect.TypeRepr.of[T]
  def targetTpe(using q: Quotes): q.reflect.TypeRepr = q.reflect.TypeRepr.of[ProtobufSerDer[T]]

object ProtobufSerDerMacros {

  sealed trait PrimitiveDataType[T]:
    def readExpr(using q:Quotes)(input: Expr[CodedInputStream]): Expr[T]
    def wireType: Int
    def defaultExpr(using q:Quotes): Expr[T]
    def isDefaultExpr(using q: Quotes)(v: Expr[T]): Expr[Boolean]
    def supportPacked: Boolean
    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v:Expr[T]): Expr[Unit]
    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[T]): Expr[Unit]

  object PrimitiveDataType:
    def of(using q:Quotes)(tpe: q.reflect.TypeRepr): Option[PrimitiveDataType[_]] =
      import q.reflect.*
      tpe match
        case x if x =:= TypeRepr.of[Boolean] => Some(Bool)
        case x if x =:= TypeRepr.of[Int] => Some(I32)
        case x if x =:= TypeRepr.of[Long] => Some(I64)
        case x if x =:= TypeRepr.of[Float] => Some(F32)
        case x if x =:= TypeRepr.of[Double] => Some(F64)
        case x if x =:= TypeRepr.of[String] => Some(STRING)
        case _ => None

  case object Bool extends PrimitiveDataType[Boolean]:
    override def readExpr(using q: Quotes)(input: Expr[CodedInputStream]): Expr[Boolean] =
      '{ ${input}.readBool() }

    override def wireType: Int = 0
    def defaultExpr(using q:Quotes): Expr[Boolean] = Expr(false)
    def isDefaultExpr(using q: Quotes)(v: Expr[Boolean]): Expr[Boolean] = '{ $v == false }
    override def supportPacked: Boolean = true
    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v:Expr[Boolean]): Expr[Unit] =
      '{ $out.writeBoolNoTag($v) }
    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[Boolean]): Expr[Unit] =
      '{ $out.writeBool($tag, $v) }

  case object I32 extends PrimitiveDataType[Int]:
    override def readExpr(using q: Quotes)(input: Expr[CodedInputStream]): Expr[Int] =
      '{ ${input}.readInt32() }

    override def wireType: Int = 0
    def defaultExpr(using q:Quotes): Expr[Int] = Expr(0)
    def isDefaultExpr(using q: Quotes)(v: Expr[Int]): Expr[Boolean] = '{ $v == 0 }
    override def supportPacked: Boolean = true

    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v: Expr[Int]): Expr[Unit] =
      '{ $out.writeInt32NoTag($v) }

    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[Int]): Expr[Unit] =
      '{ $out.writeInt32($tag, $v) }

  case object I64 extends PrimitiveDataType[Long]:
    override def readExpr(using q: Quotes)(input: Expr[CodedInputStream]): Expr[Long] =
      '{ ${ input }.readInt64() }

    override def wireType: Int = 0
    def defaultExpr(using q:Quotes): Expr[Long] = Expr(0L)
    def isDefaultExpr(using q: Quotes)(v: Expr[Long]): Expr[Boolean] = '{ $v == 0L }
    override def supportPacked: Boolean = true

    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v: Expr[Long]): Expr[Unit] =
      '{ $out.writeInt64NoTag($v) }

    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[Long]): Expr[Unit] =
      '{ $out.writeInt64($tag, $v) }

  case object F32 extends PrimitiveDataType[Float]:
    override def readExpr(using q: Quotes)(input: Expr[CodedInputStream]): Expr[Float] =
      '{ ${ input }.readFloat() }

    override def wireType: Int = 5
    def defaultExpr(using q:Quotes): Expr[Float] = Expr(0f)
    def isDefaultExpr(using q: Quotes)(v: Expr[Float]): Expr[Boolean] = '{ $v == 0.0f }
    override def supportPacked: Boolean = true

    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v: Expr[Float]): Expr[Unit] =
      '{ $out.writeFloatNoTag($v) }

    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[Float]): Expr[Unit] =
      '{ $out.writeFloat($tag, $v) }

  case object F64 extends PrimitiveDataType[Double]:
    override def readExpr(using q: Quotes)(input: Expr[CodedInputStream]): Expr[Double] =
      '{ ${ input }.readDouble() }

    override def wireType: Int = 1
    def defaultExpr(using q:Quotes): Expr[Double] = Expr(0d)
    def isDefaultExpr(using q: Quotes)(v: Expr[Double]): Expr[Boolean] = '{ $v == 0.0d }
    override def supportPacked: Boolean = true

    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v: Expr[Double]): Expr[Unit] =
      '{ $out.writeDoubleNoTag($v) }

    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[Double]): Expr[Unit] =
      '{ $out.writeDouble($tag, $v) }

  case object STRING extends PrimitiveDataType[String]:
    override def readExpr(using q: Quotes)(input: Expr[CodedInputStream]): Expr[String] =
      '{ ${ input }.readString() }

    override def wireType: Int = 2
    def defaultExpr(using q:Quotes): Expr[String] = '{null:String}
    def isDefaultExpr(using q: Quotes)(v: Expr[String]): Expr[Boolean] =  '{ $v == null }
    override def supportPacked: Boolean = false

    def writeNoTagExpr(using q: Quotes)(out: Expr[CodedOutputStream], v: Expr[String]): Expr[Unit] =
      '{ $out.writeStringNoTag($v) }

    def writeExpr(using q: Quotes)(out: Expr[CodedOutputStream], tag: Expr[Int], v: Expr[String]): Expr[Unit] =
      '{ $out.writeString($tag, $v) }

  def generateProtobufSerderImpl[T:Type](using Quotes): Expr[ProtobufSerDer[T]] =
    new ProtobufSerDerMacros(quotes).genADT[T]

  private val NO_EXPAND_ADT = new ThreadLocal[Boolean]:
    override def initialValue(): Boolean = false
  private val NOT_EXPAND_ADTS = new ThreadLocal[Int]:
    override def initialValue(): Int = 0
  private val PRINT_MACRO_CODE: Boolean = java.lang.Boolean.getBoolean("protobuf.printMacroCode")


}

class ProtobufSerDerMacros(q: Quotes):
  import ProtobufSerDerMacros.*

  private def genADT[T: Type](using Quotes): Expr[ProtobufSerDer[T]] =
    import quotes.reflect.*

    if ProtobufSerDerMacros.NO_EXPAND_ADT.get() then
      ProtobufSerDerMacros.NOT_EXPAND_ADTS.set(ProtobufSerDerMacros.NOT_EXPAND_ADTS.get() + 1)
      '{ ??? }
    else
      val types: GeneratorMap = Map(q.reflect.TypeRepr.of[T] -> new ProductGenerator[T])

      genMultiBlock[T](types)

  private type GeneratorMap = Map[q.reflect.TypeRepr, Generator[_]]

  private def genMultiBlock[T: Type](types: GeneratorMap)(using Quotes): Expr[ProtobufSerDer[T]] =
    import quotes.reflect.*
    var seq = 0
    val valSyms: Map[TypeRepr, Symbol] = types.map: (tpe, generator) =>
      seq += 1
      val sym = Symbol.newVal(Symbol.spliceOwner, s"_$seq", generator.targetTpe, Flags.Lazy, Symbol.noSymbol)
      (tpe.asInstanceOf[TypeRepr], sym)

    val refs: Map[TypeRepr, Ref] = valSyms.map: (tpe, sym) =>
      (tpe, Ref(sym))

    val valDefs: Map[TypeRepr, ValDef] = types.map: (tpe, generator) =>
      val sym: Symbol = valSyms(tpe.asInstanceOf[TypeRepr])

      val nested = sym.asQuotes
      val refs2 = refs.asInstanceOf[Map[nested.reflect.TypeRepr, nested.reflect.Ref]]
      val deriveTerm: Term = generator.generate(using nested)(deps = refs2).asTerm
      val valDef = ValDef(sym, Some(deriveTerm))
      (tpe.asInstanceOf[TypeRepr], valDef)

    val term = Block(valDefs.values.toList, refs(TypeRepr.of[T]) )

    println("generated code: " + term.show)

    term.asExpr.asInstanceOf[Expr[ProtobufSerDer[T]]]
