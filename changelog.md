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
