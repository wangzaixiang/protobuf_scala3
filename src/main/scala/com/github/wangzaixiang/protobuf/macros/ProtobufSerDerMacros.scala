package com.github.wangzaixiang.protobuf.macros

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import scala.quoted.*
import com.github.wangzaixiang.protobuf.*

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer

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
    import quotes.reflect.*

    def genEncode(instance: Expr[T], output: Expr[CodedOutputStream])(using Quotes): Expr[Unit] =
      import quotes.reflect.*
      val tpeSym = TypeTree.of[T].symbol
      val params = tpeSym.primaryConstructor.tree.asInstanceOf[DefDef].paramss.head
        .params.asInstanceOf[List[ValDef]]

      val terms = tpeSym.caseFields.zip(params).map { (field, param) =>
        val tagNum: Expr[Int] = param.symbol.annotations.find(_.tpe =:= TypeRepr.of[tag]).get.asInstanceOf[Apply].args.head.asExprOf[Int]
        genEncodeField(field, tagNum, instance, output)
      }
      Block(terms.toList, Literal(UnitConstant())).asExprOf[Unit]

    def genDecode(input: Expr[CodedInputStream])(using Quotes): Expr[T] =
      import quotes.reflect.*
      val tpeSym = TypeTree.of[T].symbol
      val params = tpeSym.primaryConstructor.tree.asInstanceOf[DefDef].paramss.head
        .params.asInstanceOf[List[ValDef]]

      val fDoneSym = Symbol.newVal(Symbol.spliceOwner, "done", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
      val fDoneValDef = ValDef(fDoneSym, Some(Literal(BooleanConstant(false))))

      val fTagSym = Symbol.newVal(Symbol.spliceOwner, "tag", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      val fTagValDef = ValDef(fTagSym, Some(Literal(IntConstant(0))))

      val terms: List[(Symbol, ValDef, Term, List[CaseDef])] = tpeSym.caseFields.zip(params).map { (field, param) =>
        val tagNum: Expr[Int] = param.symbol.annotations.find(_.tpe =:= TypeRepr.of[tag]).get.asInstanceOf[Apply].args.head.asExprOf[Int]
        genDecodeField(field, tagNum, input)
      }

      val valdefs = fDoneValDef :: fTagValDef :: terms.map(_._2)
      val whileStmt = While( '{ ${Ref(fDoneSym).asExprOf[Boolean]} == false }.asTerm,
        {
          val a = Assign( Ref(fTagSym), '{ ${input}.readTag() }.asTerm )
          val b =
            val case0 = CaseDef( Literal(IntConstant(0)), None, Assign(Ref(fDoneSym), Literal(BooleanConstant(true))) )
            Match( Ref(fTagSym), case0 :: terms.flatMap(_._4) )
          Block( a::b::Nil, Literal(UnitConstant()) )
        }
      )
      val result = Apply( Select.unique(Ref(tpeSym.companionModule), "apply"), terms.map(_._3) )

      val block = Block( valdefs:::whileStmt::Nil, result )

      block.asExprOf[T]

    def genEncodeField(using quotes:Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
      import quotes.reflect.*
      val fieldTpe = field.tree.asInstanceOf[ValDef].tpt.tpe

      genEncodePrimitiveField(field, tagNum, instance, output)
        .orElse( genEncodeSeqPrimitiveField(field,tagNum, instance, output) )
        .getOrElse:
          if fieldTpe <:< TypeRepr.of[Seq[T]] then
            genEncodeFieldSeqStruct(field, tagNum, instance, output)
          else
            report.error(s"genEncodeField: unsupported field:${field.name} type: ${fieldTpe.show} for ${TypeTree.of[T].symbol}")
            '{
              ???
            }.asTerm

    def genEncodePrimitiveField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
      import quotes.reflect.*

      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[ft] =>
          genEncodePrimitiveField0[ft](field, tagNum, instance, output)

    def genEncodePrimitiveField0[ft: Type](using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
      import quotes.reflect.*

      PrimitiveDataType.of(TypeRepr.of[ft]) match
        case Some(primitive) =>
          val dataType = primitive.asInstanceOf[PrimitiveDataType[ft]]

          val value = Select.unique(instance.asTerm, field.name).asExprOf[ft]
          val expr ='{
            if( ! ${dataType.isDefaultExpr(value)} ) ${dataType.writeExpr(output, tagNum, value)}
          }
          Some(expr.asTerm)
        case None => None

    def genEncodeSeqPrimitiveField(using quotes:Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
      import quotes.reflect.*

      val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe
      tpe match
        case AppliedType(base, args) =>
          args(0).asType match
            case '[ft] =>
              genEncodeSeqPrimitiveField0[ft](field, tagNum, instance, output)
        case _ => None

    def genEncodeSeqPrimitiveField0[ft: Type](using quotes:Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
      import quotes.reflect.*

      val dataType: PrimitiveDataType[ft] = PrimitiveDataType.of(TypeRepr.of[ft]).get.asInstanceOf[PrimitiveDataType[ft]]
      def computePacked(): Boolean = dataType.supportPacked
      def writeNoTag(out: Expr[CodedOutputStream], v: Expr[ft]): Expr[Unit] = dataType.writeNoTagExpr(out, v)
      def write(v: Expr[ft]): Expr[Unit] = dataType.writeExpr(output, tagNum, v)

      val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[ft]]
      val packed = computePacked()
      if packed then
        Some('{
          if ${value}.nonEmpty then
            val buffer = new ByteArrayOutputStream(32)
            val out2 = CodedOutputStream.newInstance(buffer)
            ${value}.foreach :v =>
              ${ writeNoTag('{out2}, '{v}) }
            out2.flush()

            ${output}.writeUInt32NoTag( (${tagNum} << 3) | 0x02 )
            ${output}.writeUInt32NoTag(buffer.size)
            ${output}.writeRawBytes(buffer.toByteArray())
        }.asTerm)
      else
        Some('{
          ${value}.foreach :v =>
            ${write('{v})}
        }.asTerm)

    def genEncodeFieldSeqStruct(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
      import quotes.reflect.*
      val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[T]]
      val self = This(Symbol.spliceOwner.owner).asExprOf[ProtobufSerDer[T]]

      '{
        ${value}.foreach :item =>
          val buffer = new ByteArrayOutputStream(32)
          val out2 = CodedOutputStream.newInstance(buffer)
          ${self}.encode(item, out2)
          out2.flush()

          ${output}.writeUInt32NoTag((${tagNum} << 3) | 0x02)
          ${output}.writeInt32NoTag(buffer.size())
          ${output}.writeRawBytes(buffer.toByteArray())
      }.asTerm

    def genDecodeField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*
      val name = field.name
      val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe
      tpe match
        case x if x =:= TypeRepr.of[Boolean] => genDecodePrimitiveField(field, tagNum, input)
        case x if x =:= TypeRepr.of[Int] => genDecodePrimitiveField(field, tagNum, input)
        case x if x =:= TypeRepr.of[Long] => genDecodePrimitiveField(field, tagNum, input)
        case x if x =:= TypeRepr.of[Float] => genDecodePrimitiveField(field, tagNum, input)
        case x if x =:= TypeRepr.of[Double] => genDecodePrimitiveField(field, tagNum, input)
        case x if x =:= TypeRepr.of[String] => genDecodePrimitiveField(field, tagNum, input)

        case x if x <:< TypeRepr.of[Seq[Boolean]] => genDecodeFieldSeq(field, tagNum, input)
        case x if x <:< TypeRepr.of[Seq[Int]] => genDecodeFieldSeq(field, tagNum, input)
        case x if x <:< TypeRepr.of[Seq[Long]] => genDecodeFieldSeq(field, tagNum, input)
        case x if x <:< TypeRepr.of[Seq[Float]] => genDecodeFieldSeq(field, tagNum, input)
        case x if x <:< TypeRepr.of[Seq[Double]] => genDecodeFieldSeq(field, tagNum, input)
        case x if x <:< TypeRepr.of[Seq[String]] => genDecodeFieldSeq(field, tagNum, input)

        case x if tpe <:< TypeRepr.of[Seq[T]] =>
          genDecodeFieldSeqStruct(field, tagNum, input)
        case _ =>
          report.error(s"genDecodeField: unsupported field:${field.name} type: ${tpe.show} for ${TypeTree.of[T].symbol}")
          ???

    def genDecodePrimitiveField(using quotes:Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*

      def defaultValue(dataType: PrimitiveDataType[_]): Term = dataType.defaultExpr.asTerm
      def readValue(dataType: PrimitiveDataType[_]): Term = dataType.readExpr(input).asTerm

      val name = field.name
      val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe
      val dataType = PrimitiveDataType.of(tpe).get
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, tpe, Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some( defaultValue(dataType) ))
      val term = Ref(symbol)
      val caseDef =
        val num = Expr.unapply(tagNum).get
        val num1 = (num << 3) | dataType.wireType
        val assign = Assign(Ref(symbol), readValue(dataType))
        CaseDef(Literal(IntConstant(num1)), None, assign)
      (symbol, valdef, term, caseDef :: Nil)

    def genDecodeFieldSeq(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*

      val seqSymbol = Symbol.requiredClass("scala.collection.immutable.Seq")

      field.tree.asInstanceOf[ValDef].tpt.tpe match
        case AppliedType(base, args) if base =:= seqSymbol.typeRef =>
          args(0).asType match
            case '[ft] =>
              genDecodeFieldSeq0[ft](field, tagNum, input)


    def genDecodeFieldSeq0[ft: Type](using quotes:Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*

      def read(): Expr[ft] = PrimitiveDataType.of(TypeRepr.of[ft]).get.readExpr(input).asInstanceOf[Expr[ft]]

      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[ft]], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some('{ ArrayBuffer[ft]() }.asTerm))
      val term = '{ ${ Ref(symbol).asExprOf[ArrayBuffer[ft]] }.toSeq }.asTerm
      val num = Expr.unapply(tagNum).get
      val num1 = num << 3   // for unpacked
      val num2 = num1 | 0x02 // for packed primitive or String
      val caseDef1: List[CaseDef] =
        val tagType = if TypeRepr.of[ft] =:= TypeRepr.of[String] then num1 | 0x02 else num1
        CaseDef(Literal(IntConstant(tagType)), None, '{ ${Ref(symbol).asExprOf[ArrayBuffer[ft]]}.append( ${read()} ) }.asTerm) :: Nil

      val caseDef2: List[CaseDef] = if !(TypeRepr.of[ft] =:= TypeRepr.of[String]) then
        val code = '{
          val length = ${ input }.readRawVarint32()
          val oldLimit = ${ input }.pushLimit(length)
          while ${ input }.getBytesUntilLimit() > 0 do
            ${ Ref(symbol).asExprOf[ArrayBuffer[ft]] }.append( ${read()} )
          ${ input }.popLimit(oldLimit)
        }.asTerm
        CaseDef(Literal(IntConstant(num2)), None, code) :: Nil
      else Nil

      (symbol, valdef, term, caseDef1 ++ caseDef2)

    def genDecodeFieldSeqStruct(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*
      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[T]], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some('{ ArrayBuffer[T]() }.asTerm))
      val term = '{ ${Ref(symbol).asExprOf[ArrayBuffer[T]]}.toSeq }.asTerm
      val num = Expr.unapply(tagNum).get
      val num1 = num << 3 | 0x02
      val self: Expr[ProtobufSerDer[T]] = This(Symbol.spliceOwner.owner).asExprOf[ProtobufSerDer[T]]
      val caseDef1 =
        val code = '{
          val len = ${input}.readInt32()
          val oldLimit = ${input}.pushLimit(len)
          val item = ${self}.decode(${input})
          ${Ref(symbol).asExprOf[ArrayBuffer[T]]}.append(item)
          ${input}.popLimit(oldLimit)
        }.asTerm
        CaseDef( Literal(IntConstant(num1)), None, code )
      (symbol, valdef, term, caseDef1::Nil)


    val result = '{
      new ProtobufSerDer[T] {
        def encode(instance: T, output: CodedOutputStream): Unit = ${ genEncode('{instance}, '{output}) }
        def decode(input: CodedInputStream): T = ${genDecode('{input}) }
      }
    }

//    println("result = " + result.show)

    result

}
