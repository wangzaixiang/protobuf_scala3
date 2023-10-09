package com.github.wangzaixiang.protobuf.macros

import scala.quoted.{Expr, *}
import com.github.wangzaixiang.protobuf.*
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer

class ProductGenerator[T: Type] extends Generator[T]{

  import ProtobufSerDerMacros.PrimitiveDataType

  private def genEncode(using quotes:Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Term], instance: Expr[T], output: Expr[CodedOutputStream]): Expr[Unit] =
    import quotes.reflect.*
    val tpeSym = TypeTree.of[T].symbol
    val params = tpeSym.primaryConstructor.tree.asInstanceOf[DefDef].paramss.head
      .params.asInstanceOf[List[ValDef]]

    val terms = tpeSym.caseFields.zip(params).map { (field, param) =>
      val tagNum: Expr[Int] = param.symbol.annotations.find(_.tpe =:= TypeRepr.of[tag]).get.asInstanceOf[Apply].args.head.asExprOf[Int]
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[ft] => genEncodeField[ft](field, tagNum, instance, output, deps)
    }
    Block(terms.toList, Literal(UnitConstant())).asExprOf[Unit]

  private def genDecode(using quotes:Quotes)(deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Term], input: Expr[CodedInputStream]): Expr[T] =
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
      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[ft] => genDecodeField[ft](field, tagNum, input, deps)
    }

    val valdefs = fDoneValDef :: fTagValDef :: terms.map(_._2)
    val whileStmt = While('{ ${ Ref(fDoneSym).asExprOf[Boolean] } == false }.asTerm,
      {
        val a = Assign(Ref(fTagSym), '{ ${ input }.readTag() }.asTerm)
        val b =
          val case0 = CaseDef(Literal(IntConstant(0)), None, Assign(Ref(fDoneSym), Literal(BooleanConstant(true))))
          Match(Ref(fTagSym), case0 :: terms.flatMap(_._4))
        Block(a :: b :: Nil, Literal(UnitConstant()))
      }
    )
    val result = Apply(Select.unique(Ref(tpeSym.companionModule), "apply"), terms.map(_._3))

    val block = Block(valdefs ::: whileStmt :: Nil, result)

    block.asExprOf[T]

  private def genEncodeField[S: Type](using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream], deps:Map[quotes.reflect.TypeRepr, quotes.reflect.Term]): quotes.reflect.Term =
    import quotes.reflect.*
    val fieldTpe = field.tree.asInstanceOf[ValDef].tpt.tpe

    def encode_Primitive: quotes.reflect.Term =
      val dataType = PrimitiveDataType.of(TypeRepr.of[S]).get.asInstanceOf[PrimitiveDataType[S]]

      val value = Select.unique(instance.asTerm, field.name).asExprOf[S]
      val expr = '{
        if (!${ dataType.isDefaultExpr(value) }) ${ dataType.writeExpr(output, tagNum, value) }
      }
      expr.asTerm

    def encode_SeqPrimitive[et: Type]: quotes.reflect.Term =
      val dataType: PrimitiveDataType[et] = PrimitiveDataType.of(TypeRepr.of[et]).get.asInstanceOf[PrimitiveDataType[et]]
      def computePacked(): Boolean = dataType.supportPacked
      def writeNoTag(out: Expr[CodedOutputStream], v: Expr[et]): Expr[Unit] = dataType.writeNoTagExpr(out, v)
      def write(v: Expr[et]): Expr[Unit] = dataType.writeExpr(output, tagNum, v)

      val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[et]]
      val packed = computePacked()
      if packed then
        '{
          if ${ value }.nonEmpty then
            val buffer = new ByteArrayOutputStream(32)
            val out2 = CodedOutputStream.newInstance(buffer)
            ${ value }.foreach: v =>
              ${ writeNoTag('{ out2 }, '{ v }) }
            out2.flush()

            ${ output }.writeUInt32NoTag((${ tagNum } << 3) | 0x02)
            ${ output }.writeUInt32NoTag(buffer.size)
            ${ output }.writeRawBytes(buffer.toByteArray)
        }.asTerm
      else
        '{
          ${ value }.foreach: v =>
            ${ write('{ v }) }
        }.asTerm

    def encode_SerDer(serder: Expr[ProtobufSerDer[S]]): quotes.reflect.Term =
      val fieldTpe = TypeRepr.of[S]

      val value = Select.unique(instance.asTerm, field.name).asExprOf[S]
      '{
        if $value != null then
          val buffer = new ByteArrayOutputStream(32)
          val out2 = CodedOutputStream.newInstance(buffer)
          $serder.encode($value, out2)
          out2.flush()

          ${ output }.writeUInt32NoTag((${ tagNum } << 3) | 0x02)
          ${ output }.writeInt32NoTag(buffer.size())
          ${ output }.writeRawBytes(buffer.toByteArray)
      }.asTerm

    def encode_SeqSerDer[et: Type](serder: Expr[ProtobufSerDer[et]]): quotes.reflect.Term =
      val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[et]]
      '{
        $value.foreach: item =>
          val buffer = new ByteArrayOutputStream(32)
          val out2 = CodedOutputStream.newInstance(buffer)
          $serder.encode(item, out2)
          out2.flush()

          ${ output }.writeUInt32NoTag((${ tagNum } << 3) | 0x02)
          ${ output }.writeInt32NoTag(buffer.size())
          ${ output }.writeRawBytes(buffer.toByteArray)
      }.asTerm


    fieldTpe match
      case x if deps contains fieldTpe => encode_SerDer(deps(fieldTpe).asExprOf[ProtobufSerDer[S]])
      case x if Expr.summon[ProtobufSerDer[S]].nonEmpty => encode_SerDer(Expr.summon[ProtobufSerDer[S]].get)
      case x if PrimitiveDataType.of(x).nonEmpty => encode_Primitive

      case x if x <:< TypeRepr.of[Seq[_]] =>
        val elemTpe = x match
          case AppliedType(base, args) => args.head

        elemTpe.asType match
          case '[et] =>
            if PrimitiveDataType.of(elemTpe).nonEmpty then encode_SeqPrimitive[et]
            else if deps contains elemTpe then
              val serder = deps(elemTpe).asExprOf[ProtobufSerDer[et]]
              encode_SeqSerDer[et](serder)
            else if Expr.summon[ProtobufSerDer[et]].nonEmpty then
              val serder = Expr.summon[ProtobufSerDer[et]].get
              encode_SeqSerDer[et](serder)
            else
              report.error(s"genEncodeField: unsupported field:${field.name} type: ${fieldTpe.show} for ${TypeTree.of[T].symbol}")
              '{
                ???
              }.asTerm

  private def genDecodeField[S:Type](using quotes: Quotes)
                            (field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream],
                             deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Term]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*
    type RESULT = (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef])

    def decode_Primitive: RESULT =
      def defaultValue(dataType: PrimitiveDataType[_]): Term = dataType.defaultExpr.asTerm
      def readValue(dataType: PrimitiveDataType[_]): Term = dataType.readExpr(input).asTerm

      val name = field.name
      val tpe = TypeRepr.of[S]
      val dataType = PrimitiveDataType.of(tpe).get
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, tpe, Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some(defaultValue(dataType)))
      val term = Ref(symbol)
      val caseDef =
        val num = Expr.unapply(tagNum).get
        val num1 = (num << 3) | dataType.wireType
        val assign = Assign(Ref(symbol), readValue(dataType))
        CaseDef(Literal(IntConstant(num1)), None, assign)
      (symbol, valdef, term, caseDef :: Nil)

    def decode_SeqPrimitive[et: Type]: RESULT =
      def read(): Expr[et] = PrimitiveDataType.of(TypeRepr.of[et]).get.readExpr(input).asInstanceOf[Expr[et]]

      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[et]], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some('{ ArrayBuffer[et]() }.asTerm))
      val term = '{ ${ Ref(symbol).asExprOf[ArrayBuffer[et]] }.toSeq }.asTerm
      val num = Expr.unapply(tagNum).get
      val num1 = num << 3 // for unpacked
      val num2 = num1 | 0x02 // for packed primitive or String
      val caseDef1: List[CaseDef] =
        val tagType = if TypeRepr.of[et] =:= TypeRepr.of[String] then num1 | 0x02 else num1
        CaseDef(Literal(IntConstant(tagType)), None, '{ ${ Ref(symbol).asExprOf[ArrayBuffer[et]] }.append(${ read() }) }.asTerm) :: Nil

      val caseDef2: List[CaseDef] = if !(TypeRepr.of[et] =:= TypeRepr.of[String]) then
        val code = '{
          val length = ${ input }.readRawVarint32()
          val oldLimit = ${ input }.pushLimit(length)
          while ${ input }.getBytesUntilLimit() > 0 do
            ${ Ref(symbol).asExprOf[ArrayBuffer[et]] }.append(${ read() })
          ${ input }.popLimit(oldLimit)
        }.asTerm
        CaseDef(Literal(IntConstant(num2)), None, code) :: Nil
      else Nil

      (symbol, valdef, term, caseDef1 ++ caseDef2)

    def decode_SeqSerDer[et: Type] (elemSerder: Expr[ProtobufSerDer[et]]): RESULT =
      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[et]], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some('{ ArrayBuffer[et]() }.asTerm))
      val term = '{ ${ Ref(symbol).asExprOf[ArrayBuffer[et]] }.toSeq }.asTerm
      val num = Expr.unapply(tagNum).get
      val num1 = num << 3 | 0x02
      val caseDef1 =
        val code = '{
          val len = ${ input }.readInt32()
          val oldLimit = ${ input }.pushLimit(len)
          val item = $elemSerder.decode(${ input })
          ${ Ref(symbol).asExprOf[ArrayBuffer[et]] }.append(item)
          ${ input }.popLimit(oldLimit)
        }.asTerm
        CaseDef(Literal(IntConstant(num1)), None, code)
      (symbol, valdef, term, caseDef1 :: Nil)

    def decode_SerDer (serder: Expr[ProtobufSerDer[S]]): RESULT =
      val name = field.name
      val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[S], Flags.Mutable, Symbol.noSymbol)
      val valdef = ValDef(symbol, Some(Literal(NullConstant())))
      val term = Ref(symbol)
      val num = Expr.unapply(tagNum).get
      val num1 = num << 3 | 0x02
      val caseDef1 =
        val read = '{
          val len = ${ input }.readInt32()
          val oldLimit = ${ input }.pushLimit(len)
          val result = $serder.decode(${ input })
          ${ input }.popLimit(oldLimit)
          result
        }.asTerm
        val code = Assign(term, read)
        CaseDef(Literal(IntConstant(num1)), None, code)
      (symbol, valdef, term, caseDef1 :: Nil)


    val name = field.name
    val tpe = TypeRepr.of[S]

    val lookup: Option[Expr[ProtobufSerDer[S]]] = Expr.summon[ProtobufSerDer[S]]

    tpe match
      case x if deps.contains(tpe) => decode_SerDer(deps(tpe).asExprOf[ProtobufSerDer[S]])
      case x if lookup.nonEmpty => decode_SerDer(lookup.get)

      case x if PrimitiveDataType.of(x).nonEmpty => decode_Primitive
      case x if x <:< TypeRepr.of[Seq[_]] =>
        val elemTpe = x match
          case AppliedType(base, args) => args.head

        elemTpe.asType match
          case '[et] =>
            if PrimitiveDataType.of(TypeRepr.of[et]).nonEmpty then decode_SeqPrimitive[et]
            else if deps contains elemTpe then
              val serder = deps(elemTpe).asExprOf[ProtobufSerDer[et]]
              decode_SeqSerDer[et](serder)
            else Expr.summon[ProtobufSerDer[et]] match
              case Some(serder) =>   decode_SeqSerDer[et](serder)
              case None =>
                report.error(s"genDecodeField: unsupported field:${field.name} type: ${tpe.show} for ${TypeTree.of[T].symbol}")
                ???
      case _ =>
        report.error(s"genDecodeField: unsupported field:${field.name} type: ${tpe.show} for ${TypeTree.of[T].symbol}")
        ???


  override def generate(using q: Quotes)(deps: Map[q.reflect.TypeRepr, q.reflect.Term]): Expr[ProtobufSerDer[T]] =
    import q.reflect.*

    val result = '{
      new ProtobufSerDer[T] {
        def encode(instance: T, output: CodedOutputStream): Unit = ${ genEncode(deps.asInstanceOf[Map[quotes.reflect.TypeRepr, quotes.reflect.Term]], '{ instance }, '{ output }) }

        def decode(input: CodedInputStream): T = ${ genDecode(deps.asInstanceOf[Map[quotes.reflect.TypeRepr, quotes.reflect.Term]], '{ input }) }
      }
    }

    result
}
