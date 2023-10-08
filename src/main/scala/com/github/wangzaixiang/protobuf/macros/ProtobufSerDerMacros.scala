package com.github.wangzaixiang.protobuf.macros

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import scala.quoted.*
import com.github.wangzaixiang.protobuf.*

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer

object ProtobufSerDerMacros {

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
      println("block = " + block.show)

      //      '{
      //        var done = false
      //        ???
      //      }
      block.asExprOf[T]

    def genEncodeField(using quotes:Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
      import quotes.reflect.*
      val fieldTpe = field.tree.asInstanceOf[ValDef].tpt.tpe
      if fieldTpe =:= TypeRepr.of[Int] then
        genEncodeFieldInt(field, tagNum, instance, output)
      else if fieldTpe <:< TypeRepr.of[Seq[Int]] then
        genEncodeFieldSeqInt(field, tagNum, instance, output)
      else if fieldTpe <:< TypeRepr.of[Seq[T]] then
        genEncodeFieldSeqStruct(field, tagNum, instance, output)
      else
        '{
          ???
        }.asTerm

    def genEncodeFieldInt(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
      import quotes.reflect.*
      val fieldName = field.name
      val valueTerm = Select.unique(instance.asTerm, fieldName).asExprOf[Int]
      '{
        if ${valueTerm} != 0 then
          ${output}.writeInt32(${tagNum}, ${valueTerm})
      }.asTerm

    def genEncodeFieldSeqInt(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
      import quotes.reflect.*
      val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[Int]]
      '{
        if ${value}.nonEmpty then
          val buffer = new ByteArrayOutputStream(32)
          val out2 = CodedOutputStream.newInstance(buffer)
          ${value}.foreach :v =>
            out2.writeInt32NoTag(v)
          out2.flush()

          ${output}.writeUInt32NoTag((${tagNum} << 3) | 0x02)
          ${output}.writeUInt32NoTag(buffer.size())
          ${output}.writeRawBytes(buffer.toByteArray())

      }.asTerm

    def genEncodeFieldSeqStruct(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
      import quotes.reflect.*
      val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[T]]
      println(s"spliceOwner = ${Symbol.spliceOwner.owner}")
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
      if tpe =:= TypeRepr.of[Int] then
        genDecodeFieldInt(field, tagNum, input)
      else if tpe <:< TypeRepr.of[Seq[Int]] then
        genDecodeFieldSeqInt(field, tagNum, input)
      else if tpe <:< TypeRepr.of[Seq[T]] then
        genDecodeFieldSeqStruct(field, tagNum, input)
      else
        ???

    def genDecodeFieldInt(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*
      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some(Literal(IntConstant(0))))
      val term = Ref(symbol)
      val caseDef =
        val num = Expr.unapply(tagNum).get
        val num1 = num << 3
        val assign = Assign(Ref(symbol), '{ ${input}.readInt32() }.asTerm)
        CaseDef( Literal(IntConstant(num1)), None, assign)
      (symbol, valdef, term, caseDef::Nil)

    def genDecodeFieldSeqInt(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
      import quotes.reflect.*
      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[Int]], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some('{ ArrayBuffer[Int]() }.asTerm))
      val term = '{ ${Ref(symbol).asExprOf[ArrayBuffer[Int]]}.toSeq }.asTerm
      val num = Expr.unapply(tagNum).get
      val num1 = num << 3
      val num2 = num1 | 0x02
      val caseDef1 =
        CaseDef( Literal(IntConstant(num1)), None, '{ ${Ref(symbol).asExprOf[ArrayBuffer[Int]]}.append(${input}.readInt32()) }.asTerm )
      val caseDef2 =
        val code = '{
          val length = ${input}.readRawVarint32()
          val oldLimit = ${input}.pushLimit(length)
          while ${input}.getBytesUntilLimit() > 0 do
            ${Ref(symbol).asExprOf[ArrayBuffer[Int]]}.append(${input}.readInt32())
          ${input}.popLimit(oldLimit)
        }.asTerm

        CaseDef( Literal(IntConstant(num2)), None, code )
      (symbol, valdef, term, caseDef1::caseDef2::Nil)

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

    println("generate result = " + result.show)

    result

}
