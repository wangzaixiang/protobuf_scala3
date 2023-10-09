# Scala3 Binding for protobuf

## why another binding than protobuf-java?
protobuf 官方提供了 Java 绑定， 处于对 protobuf-java 实现中生成的代码的恐惧，以及对清晰简洁代码的渴望，我编写了这个项目。

由于 Scala 提供了强大的 Macro 编程能力，这也使得在 Scala 中 protobuf 变得非常简洁：

```scala 3
    import com.github.wangzaixiang.protobuf4s.*

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
    
    val encoded: Array[Byte] = bean.toBytes
    val decoded: SimpleBean = encoded.toBean[SimpleBean]

```

清爽的世界，无比美好（除了 @tag 还是有些多余）

## TODO List
- [ ] datatype
  - [X] primitive
  - [X] repeat field
  - [ ] optional field (with default value)
  - [ ] enum 
  - [ ] map
  - [ ] oneof
- [ ] protoc plugin to generate clean Scala code for proto file
- [ ] generate metadata(google.protobuf.descriptor).