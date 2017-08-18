# Bytes

A library for fast immutable `Bytes`. The type is built on top of `Core`'s
immutable `Array` type limited to values of `Int` in the range of `0` - `255`.

To use, simply import the main namespace:

    import Bytes

or specific function(s):

    import Bytes
        exposing
            ( Byte
            , Bytes
            , Hex(..)
            , and
            , append
            , byte
            , empty
            , fromBytes
            , fromHex
            , fromList
            , fromURI
            , fromUTF8
            , isBytes
            , isEmpty
            , isHex
            , length
            , map
            , or
            , repeat
            , toArray
            , toList
            , toString
            , xor
            )
