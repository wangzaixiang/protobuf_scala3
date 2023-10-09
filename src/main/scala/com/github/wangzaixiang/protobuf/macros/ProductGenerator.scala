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
      genEncodeField(field, tagNum, instance, output, deps)
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
      genDecodeField(field, tagNum, input, deps)
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

  private def genEncodeField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream], deps:Map[quotes.reflect.TypeRepr, quotes.reflect.Term]): quotes.reflect.Term =
    import quotes.reflect.*
    val fieldTpe = field.tree.asInstanceOf[ValDef].tpt.tpe

    genEncodeField_HavingSerDer(field, tagNum, instance, output, deps)
      .orElse( genEncodePrimitiveField(field, tagNum, instance, output) )
      .orElse(genEncodeSeqPrimitiveField(field, tagNum, instance, output))
      .getOrElse:
        if fieldTpe <:< TypeRepr.of[Seq[T]] then
          genEncodeFieldSeqStruct(field, tagNum, instance, output)
        else
          report.error(s"genEncodeField: unsupported field:${field.name} type: ${fieldTpe.show} for ${TypeTree.of[T].symbol}")
          '{
            ???
          }.asTerm

  private def genEncodeField_HavingSerDer(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream], deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Term]): Option[quotes.reflect.Term] =
    import quotes.reflect.*

    val fieldTpe = field.tree.asInstanceOf[ValDef].tpt.tpe
    val serder =
      if deps.contains(fieldTpe) then
        deps.get(fieldTpe)
      else
        fieldTpe.asType match
          case '[ft] =>
            Expr.summon[ProtobufSerDer[ft]].map(_.asTerm)

    serder match
      case Some(term) =>
        fieldTpe.asType match
          case '[ft] =>
            val value = Select.unique(instance.asTerm, field.name).asExprOf[ft]
            val serder = term.asExprOf[ProtobufSerDer[ft]]
            Some('{
              if $value != null then
                val buffer = new ByteArrayOutputStream(32)
                val out2 = CodedOutputStream.newInstance(buffer)
                $serder.encode($value, out2)
                out2.flush()

                ${ output }.writeUInt32NoTag((${ tagNum } << 3) | 0x02)
                ${ output }.writeInt32NoTag(buffer.size())
                ${ output }.writeRawBytes(buffer.toByteArray())
            }.asTerm)
      case None => None

  private def genEncodePrimitiveField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
    import quotes.reflect.*

    field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
      case '[ft] =>
        genEncodePrimitiveField0[ft](field, tagNum, instance, output)

  private def genEncodePrimitiveField0[ft: Type](using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
    import quotes.reflect.*

    PrimitiveDataType.of(TypeRepr.of[ft]) match
      case Some(primitive) =>
        val dataType = primitive.asInstanceOf[PrimitiveDataType[ft]]

        val value = Select.unique(instance.asTerm, field.name).asExprOf[ft]
        val expr = '{
          if (!${ dataType.isDefaultExpr(value) }) ${ dataType.writeExpr(output, tagNum, value) }
        }
        Some(expr.asTerm)
      case None => None

  private def genEncodeSeqPrimitiveField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
    import quotes.reflect.*

    val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe
    tpe match
      case AppliedType(base, args) =>
        args(0).asType match
          case '[ft] =>
            genEncodeSeqPrimitiveField0[ft](field, tagNum, instance, output)
      case _ => None

  private def genEncodeSeqPrimitiveField0[ft: Type](using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): Option[quotes.reflect.Term] =
    import quotes.reflect.*

    val dataType: PrimitiveDataType[ft] = PrimitiveDataType.of(TypeRepr.of[ft]).get.asInstanceOf[PrimitiveDataType[ft]]

    def computePacked(): Boolean = dataType.supportPacked

    def writeNoTag(out: Expr[CodedOutputStream], v: Expr[ft]): Expr[Unit] = dataType.writeNoTagExpr(out, v)

    def write(v: Expr[ft]): Expr[Unit] = dataType.writeExpr(output, tagNum, v)

    val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[ft]]
    val packed = computePacked()
    if packed then
      Some('{
        if ${ value }.nonEmpty then
          val buffer = new ByteArrayOutputStream(32)
          val out2 = CodedOutputStream.newInstance(buffer)
          ${ value }.foreach: v =>
            ${ writeNoTag('{ out2 }, '{ v }) }
          out2.flush()

          ${ output }.writeUInt32NoTag((${ tagNum } << 3) | 0x02)
          ${ output }.writeUInt32NoTag(buffer.size)
          ${ output }.writeRawBytes(buffer.toByteArray())
      }.asTerm)
    else
      Some('{
        ${ value }.foreach: v =>
          ${ write('{ v }) }
      }.asTerm)

  private def genEncodeFieldSeqStruct(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], instance: Expr[T], output: Expr[CodedOutputStream]): quotes.reflect.Term =
    import quotes.reflect.*
    val value = Select.unique(instance.asTerm, field.name).asExprOf[Seq[T]]
    val self = This(Symbol.spliceOwner.owner).asExprOf[ProtobufSerDer[T]]

    '{
      ${ value }.foreach: item =>
        val buffer = new ByteArrayOutputStream(32)
        val out2 = CodedOutputStream.newInstance(buffer)
        ${ self }.encode(item, out2)
        out2.flush()

        ${ output }.writeUInt32NoTag((${ tagNum } << 3) | 0x02)
        ${ output }.writeInt32NoTag(buffer.size())
        ${ output }.writeRawBytes(buffer.toByteArray())
    }.asTerm

  private def genDecodeField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream], deps: Map[quotes.reflect.TypeRepr, quotes.reflect.Term]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*
    val name = field.name
    val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe

    val lookup: Option[Expr[ProtobufSerDer[_]]] =
      tpe.asType match
        case '[ft] =>
          Expr.summon[ProtobufSerDer[ft]]

    tpe match
      case x if deps.contains(tpe) => genDecodeField_WithSerDer(field, tagNum, input, deps(tpe))
      case x if lookup.nonEmpty => genDecodeField_WithSerDer(field, tagNum, input, lookup.get.asTerm)
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

  private def genDecodePrimitiveField(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*

    def defaultValue(dataType: PrimitiveDataType[_]): Term = dataType.defaultExpr.asTerm

    def readValue(dataType: PrimitiveDataType[_]): Term = dataType.readExpr(input).asTerm

    val name = field.name
    val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe
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

  private def genDecodeFieldSeq(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*

    val seqSymbol = Symbol.requiredClass("scala.collection.immutable.Seq")

    field.tree.asInstanceOf[ValDef].tpt.tpe match
      case AppliedType(base, args) if base =:= seqSymbol.typeRef =>
        args(0).asType match
          case '[ft] =>
            genDecodeFieldSeq0[ft](field, tagNum, input)

  private def genDecodeFieldSeq0[ft: Type](using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*

    def read(): Expr[ft] = PrimitiveDataType.of(TypeRepr.of[ft]).get.readExpr(input).asInstanceOf[Expr[ft]]

    val name = field.name
    val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[ft]], Flags.Mutable, Symbol.noSymbol)
    val valdef = ValDef(symbol, Some('{ ArrayBuffer[ft]() }.asTerm))
    val term = '{ ${ Ref(symbol).asExprOf[ArrayBuffer[ft]] }.toSeq }.asTerm
    val num = Expr.unapply(tagNum).get
    val num1 = num << 3 // for unpacked
    val num2 = num1 | 0x02 // for packed primitive or String
    val caseDef1: List[CaseDef] =
      val tagType = if TypeRepr.of[ft] =:= TypeRepr.of[String] then num1 | 0x02 else num1
      CaseDef(Literal(IntConstant(tagType)), None, '{ ${ Ref(symbol).asExprOf[ArrayBuffer[ft]] }.append(${ read() }) }.asTerm) :: Nil

    val caseDef2: List[CaseDef] = if !(TypeRepr.of[ft] =:= TypeRepr.of[String]) then
      val code = '{
        val length = ${ input }.readRawVarint32()
        val oldLimit = ${ input }.pushLimit(length)
        while ${ input }.getBytesUntilLimit() > 0 do
          ${ Ref(symbol).asExprOf[ArrayBuffer[ft]] }.append(${ read() })
        ${ input }.popLimit(oldLimit)
      }.asTerm
      CaseDef(Literal(IntConstant(num2)), None, code) :: Nil
    else Nil

    (symbol, valdef, term, caseDef1 ++ caseDef2)

  private def genDecodeFieldSeqStruct(using quotes: Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream]): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*
    val name = field.name
    val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ArrayBuffer[T]], Flags.Mutable, Symbol.noSymbol)
    val valdef = ValDef(symbol, Some('{ ArrayBuffer[T]() }.asTerm))
    val term = '{ ${ Ref(symbol).asExprOf[ArrayBuffer[T]] }.toSeq }.asTerm
    val num = Expr.unapply(tagNum).get
    val num1 = num << 3 | 0x02
    val self: Expr[ProtobufSerDer[T]] = This(Symbol.spliceOwner.owner).asExprOf[ProtobufSerDer[T]]
    val caseDef1 =
      val code = '{
        val len = ${ input }.readInt32()
        val oldLimit = ${ input }.pushLimit(len)
        val item = ${ self }.decode(${ input })
        ${ Ref(symbol).asExprOf[ArrayBuffer[T]] }.append(item)
        ${ input }.popLimit(oldLimit)
      }.asTerm
      CaseDef(Literal(IntConstant(num1)), None, code)
    (symbol, valdef, term, caseDef1 :: Nil)

  private def genDecodeField_WithSerDer(using Quotes)(field: quotes.reflect.Symbol, tagNum: Expr[Int], input: Expr[CodedInputStream], serder: quotes.reflect.Term): (quotes.reflect.Symbol, quotes.reflect.ValDef, quotes.reflect.Term, List[quotes.reflect.CaseDef]) =
    import quotes.reflect.*

    field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
      case '[ft] =>
        val name = field.name
        val symbol = Symbol.newVal(Symbol.spliceOwner, "f_" + name, TypeRepr.of[ft], Flags.Mutable, Symbol.noSymbol)
        val valdef = ValDef(symbol, Some(Literal(NullConstant())))
        val term = Ref(symbol)
        val num = Expr.unapply(tagNum).get
        val num1 = num << 3 | 0x02
        val serderExpr = serder.asExpr.asInstanceOf[Expr[ProtobufSerDer[ft]]]
        val caseDef1 =
          val read = '{
            val len = ${ input }.readInt32()
            val oldLimit = ${ input }.pushLimit(len)
            val result = $serderExpr.decode(${ input })
            ${ input }.popLimit(oldLimit)
            result
          }.asTerm
          val code = Assign(term, read)
          CaseDef(Literal(IntConstant(num1)), None, code)
        (symbol, valdef, term, caseDef1 :: Nil)


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
