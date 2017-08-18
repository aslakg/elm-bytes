# 1.2.0

### MINOR change

  * Added the following types to `Bytes`:

  Byte - represent a single byte, useful for safe byte to byte transforms

  Hex -> hexadecimal digits, used to create a single byte in a type safe way

  * Added the following functions to `Bytes`:

  repeat : Int -> Byte -> Bytes

  append : Bytes -> Bytes -> Bytes

  byte : Hex -> Hex -> Byte

  map : (Byte -> Byte) -> Bytes -> Bytes

  and : Byte -> Byte -> Byte

  or : Byte -> Byte -> Byte

  xor : Byte -> Byte -> Byte

# 1.1.0

### MINOR change

  * Added the following functions to `Bytes`:

	fromURI : String -> Bytes

# 1.0.0

### Initial release

  * Added the following functions to `Bytes`:

	empty : Bytes

	fromList : List Int -> Result String Bytes

	fromBytes : String -> Bytes

	fromUTF8 : String -> Bytes

	fromHex : String -> Bytes

	isBytes : String -> Bool

	isEmpty : Bytes -> Bool

	isHex : String -> Bool

	length : Bytes -> Int

	toArray : Bytes -> Array Int

	toList : Bytes -> List Int

	toString : Bytes -> String
