# 1.2.0

### MINOR change

  * Added the following functions to `Bytes`:
  
  append : Bytes -> Bytes -> Bytes

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
