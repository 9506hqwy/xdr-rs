# XDR (RFC 4506) for Serde

This crate provides XDR (RFC 4506) serializer and deserializer using [serde](https://github.com/serde-rs/serde),
and stub code generation tool from interface defined file.

## Usage

```rust
let bytes = serde_xdr::to_bytes(&1u8)?;
let val: u8 = serde_xdr::from_bytes(bytes)?;
```

## Data Types

| XDR                         | Rust/Serde                                     |
| --------------------------- | ---------------------------------------------- |
| Integer                     | i8, i16, i32                                   |
| Unsigned Integer            | u8, u16, u32, bool, char                       |
| Enumeration                 | unit_variant                                   |
| Hyper Integer               | i64                                            |
| Unsigned Hyper Integer      | u64                                            |
| Floatinng-Point             | f32                                            |
| Double-Precision            | f64                                            |
| Quadruple-Precision         |                                                |
| Fixed-Length Opaque Data    | [u8; N] [^1]                                   |
| Variable-Length Opaque Data | Vec\<u8> [^2]                                  |
| String                      | string                                         |
| Fixed-Length Arary          | [T; N]                                         |
| Variable-Length Array       | seq, map                                       |
| Structure                   | newtype_struct, tuple, tuple_struct, struct    |
| Discriminated Union         | newtype_variant, tuple_variant, struct_variant |
| Void                        | unit, unit_struct                              |
| Optional-Data               | Option\<T>                                     |

[^1]: need ` #[serde(with = "serde_xdr::opaque::fixed")]`.
[^2]: need ` #[serde(with = "serde_xdr::opaque::variable")]`.

### Enumeration and Discriminated Union

XDR enumeration and union is mapped to bellow pattern rust code.

```
enum { RED = 2, YELLOW = 3, BLUE = 5 } colors;
```

1. Complements name-identifier.

   ```rust
   #[derive(Deserialize, Serialize)]
   enum Colors {
       _Reserved0
       _Reserved1,
       Red,
       Yellow,
       _Reserved4,
       Blue,
   }
   ```

2. Implements `TryFrom<i32>` (Enumeration only).

   ```rust
   #[derive(Deserialize, Serialize)]
   enum Colors {
       Red,
       Yellow,
       Blue,
   }
   
   impl TryFrom<i32> for Colors {
       type Error = serde_xdr::error::Error;
   
       fn try_from(v: i32) -> Result<Self, Self::Error> {
           match v {
               2 => Colors::Red,
               3 => Colors::Yellow,
               5 => Colors::Blue,
               _ => unreachable!(),
           }
       }
   }
   
   impl AsRef<i32> for Colors {
       fn as_ref(&self) -> &'static i32 {
           match self {
               Colors::Red => 2,
               Colors::Yellow => 3,
               Colors::Blue => 5,
           }
       }
   }
   ```

   This pattern needs to implements `#[serde(with = "serde_xdr::primitive::signed32")]` attribute to struct properties.

3. Implements `XdrIndexer`.

   ```rust
   #[derive(XdrIndexer)]
   enum Colors {
       Red,
       Yellow,
       Blue,
   }
   
   impl XdrIndexer for Colors {
       type Error = serde_xdr::error::Error;
   
       fn name_by_index(index: i32) -> Result<&'static str, Self::Error> {
           2 => Ok("Red"),
           3 => Ok("Yellow"),
           5 => Ok("Blue"),
           _ => unreachable!(),
       }
   
       fn index(&self) -> i32 {
           Colors::Red => 2,
           Colors::Yellow => 3,
           Colors::Blue => 5,
       }
   }
   ```

   This pattern is instead of `Serialize` and `Deserialize` attribute.

## Stub Code Generation

```sh
rpcgen [--use-std-trait] [--use-extra-trait] [--use-indexer] <PATH>
```

PATH is interface defined file path.

If `--use-std-trait` is specified,
XDR `enum` implements `TryFrom<i32>` and `#[serde(with = "serde_xdr::primitive::signed32")]` attribute.

If `--use-extra-trait` is specified, XDR `union` implements `XdrIndexer` attribute.

If `--use-indexer` is pecified, XDR `enum` and `union` implements `XdrIndexer` attribute.

## Examples

- [libvirt-remote-rs](https://github.com/9506hqwy/libvirt-remote-rs)
- [nfs-client-rs](https://github.com/9506hqwy/nfs-client-rs)

## References

- [XDR: External Data Representation Standard](https://www.rfc-editor.org/rfc/rfc4506.txt)
