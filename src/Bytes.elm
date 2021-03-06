module Bytes
    exposing
        ( Bytes
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
        , toArray
        , toList
        , toString
        )

{-| A library for fast immutable `Bytes`. The type is built on top of `Core`'s
immutable `Array` type limited to values of `Int` in the range of `0` - `255`.

# Bytes
@docs Bytes

# Creating Bytes

@docs empty, fromBytes, fromHex, fromList, fromURI, fromUTF8

# Basics

@docs isBytes, isEmpty, isHex, length

# Bytes to Array, List or String

@docs toArray, toList, toString

-}

--LIBS

import Array exposing (Array)
import Bitwise exposing (and, or, shiftRightBy)
import Char exposing (KeyCode, fromCode, isHexDigit, toCode)
import Regex exposing (HowMany(All), regex, replace)
import String exposing (fromChar, toList)


{-| Representation of fast immutable `Bytes`. The `ByteArray` constructor is not
exposed from the module which ensures that `Bytes` will always hold valid `Int`
values in the range of `0` - `255`.
-}
type Bytes
    = ByteArray (Array Int)


{-| Returns an empty `Bytes` container. To be used with `Result.withDefault`:

    Bytes.empty
        == (Bytes.fromList [256] |> Result.withDefault empty)
-}
empty : Bytes
empty =
    [] |> Array.fromList |> ByteArray


{-| Converts a `String` (`unescape(encodeURI("foo"))`), where each `Char`
represents a single `Byte`, to `Bytes`. It's your responsability to ensure that
the `String` complies with this constraint, see `Bytes.isBytes`:

    Bytes.fromBytes "Ã¦ Ã¸ Ã¥ Ã±"
        == Bytes.fromList [195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177]
-}
fromBytes : String -> Bytes
fromBytes str =
    str
        |> String.toList
        |> List.map (\c -> c |> toCode)
        |> Array.fromList
        |> ByteArray


{-| Converts a `String`, where each `(Char, Char)` tuple represents a single
`Hex` number, to `Bytes`. It's your responsability to ensure that the `String`
complies with this constraint, see `Bytes.isHex`:

    Bytes.fromHex "7FFF"
        == Bytes.fromList [127, 255]

    Bytes.fromHex "7fff"
        == Bytes.fromList [127, 255]
-}
fromHex : String -> Bytes
fromHex str =
    str |> hexToString |> fromBytes


{-| Converts a `List` of `Int` to `Bytes` if they are all in the range `0 - 255`
otherwise it will return and error message:

    Bytes.fromList [256]
        == Err "Invalid numbers! Numbers must be between 0 and 255"

    ((Bytes.fromList [255] |> Bytes.toList) == [255])
        == True
-}
fromList : List Int -> Result String Bytes
fromList numbers =
    case List.all isByte numbers of
        True ->
            numbers |> Array.fromList |> ByteArray |> Ok

        False ->
            Err "Invalid numbers! Numbers must be between 0 and 255"


{-| Converts an `URI` (Uniform Resource Identifier) `String` to `Bytes`:

    Bytes.fromUri "%C3%A6%20%C3%B8%20%C3%A5%20%C3%B1"
        == Bytes.fromList [195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177]
-}
fromURI : String -> Bytes
fromURI str =
    str
        |> Regex.find All (Regex.regex "%[a-zA-Z0-9][a-zA-Z0-9]|.")
        |> List.map
            (\{ match } ->
                case match |> String.startsWith "%" of
                    True ->
                        match |> String.dropLeft 1 |> hexToString

                    False ->
                        match
            )
        |> List.foldl (\c a -> a ++ c) ""
        |> fromBytes


{-| Converts an `UTF-8` `String` to `Bytes`:

    Bytes.fromUTF8 "æ ø å ñ"
        == Bytes.fromList [195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177]
-}
fromUTF8 : String -> Bytes
fromUTF8 str =
    let
        two =
            -- U+0080 - U+07FF => 2 bytes 110yyyyy, 10zzzzzz
            "[\\u0080-\\u07ff]"

        three =
            -- U+0800 - U+FFFF => 3 bytes 1110xxxx, 10yyyyyy, 10zzzzzz
            "[\\u0800-\\uffff]"
    in
        str
            |> unescape two twoMultiToSingle
            |> unescape three threeMultiToSingle
            |> fromBytes


{-| Determine if each `Char` in a `String` represents a single `Byte`:

    Bytes.isBytes "Ã¦ Ã¸ Ã¥ Ã±"
        |> Expect.equal True

-}
isBytes : String -> Bool
isBytes str =
    str
        |> String.toList
        |> List.map toCode
        |> List.all isByte


{-| Determine if the `Bytes` container is empty:

    Bytes.isEmpty Bytes.empty
        |> Expect.equal True
-}
isEmpty : Bytes -> Bool
isEmpty (ByteArray numbers) =
    numbers |> Array.isEmpty


{-| Determine if all `Char` in a `String` are `HexDigits`:

    Bytes.isHex "7FFF"
        |> Expect.equal True
-}
isHex : String -> Bool
isHex str =
    str
        |> String.toList
        |> List.all isHexDigit


{-| The number of the `Bytes`:

    Bytes.fromList
        [ 127, 255 ]
        |> Result.withDefault empty
        |> Bytes.length
        |> Expect.equal 2
-}
length : Bytes -> Int
length (ByteArray numbers) =
    numbers |> Array.length


{-| Creates an `Array` of `Int` from `Bytes`:

    Bytes.fromList
        [ 127, 255 ]
        |> Result.withDefault empty
        |> Bytes.toArray
        |> Expect.equal ([ 127, 255 ] |> Array.fromList)
-}
toArray : Bytes -> Array Int
toArray (ByteArray numbers) =
    numbers


{-| Creates a `List` of `Int` from `Bytes`:

    Bytes.fromList
        [ 127, 255 ]
        |> Result.withDefault empty
        |> Bytes.toList
        |> Expect.equal [ 127, 255 ]
-}
toList : Bytes -> List Int
toList (ByteArray numbers) =
    numbers |> Array.toList


{-| Creates a `String` (`unescape(encodeURI("foo"))`), where each `Char`
represents a single `Byte`, from `Bytes`:

    Bytes.fromList
        [ 195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177 ]
        |> Result.withDefault empty
        |> Bytes.toString
        |> Expect.equal "Ã¦ Ã¸ Ã¥ Ã±"
-}
toString : Bytes -> String
toString (ByteArray numbers) =
    numbers
        |> Array.map stringify
        |> Array.foldl (\c a -> a ++ c) ""



-- HELPERS


hexToString : String -> String
hexToString hex =
    -- Take chunks of 2 hex digits and apply formula: (a * 16^1) + (b * 16^0)
    -- Example (d_16 = 13_10): 0xd3 => (d * 16^1) + (3 * 16^0) = 208 + 3 = 211
    let
        zero =
            hex |> String.all (\c -> c == '0')

        asciiToNumber x =
            let
                y =
                    x |> Char.toCode
            in
                case y > 96 of
                    -- ASCII codes for [a-f] => [97-102]
                    True ->
                        y - 97 + 10

                    -- ASCII codes for [0-9] => [48-57]
                    False ->
                        y - 48

        rec xs acc =
            case xs == "" of
                False ->
                    let
                        ( y, ys ) =
                            case String.uncons xs of
                                Nothing ->
                                    ( Nothing, "" )

                                Just ( a, rest ) ->
                                    ( Just a, rest )

                        ( z, zs ) =
                            case String.uncons ys of
                                Nothing ->
                                    ( Nothing, "" )

                                Just ( b, rest ) ->
                                    ( Just b, rest )

                        keyCode =
                            case ( y, z ) of
                                ( Just a, Just b ) ->
                                    ((asciiToNumber a) * 16)
                                        + (asciiToNumber b)

                                ( Just a, _ ) ->
                                    (asciiToNumber a) * 16

                                ( _, _ ) ->
                                    0
                    in
                        rec zs (acc ++ (keyCode |> stringify))

                True ->
                    acc
    in
        case zero of
            True ->
                ""

            False ->
                rec (hex |> String.toLower) ""


isByte : Int -> Bool
isByte n =
    n > -1 && n < 256


stringify : KeyCode -> String
stringify =
    fromCode >> fromChar


twoMultiToSingle : Char -> String
twoMultiToSingle c =
    let
        cc =
            c |> toCode

        t1 =
            cc
                |> Bitwise.shiftRightBy 6
                |> Bitwise.or 0xC0
                |> stringify

        t2 =
            cc
                |> Bitwise.and 0x3F
                |> Bitwise.or 0x80
                |> stringify
    in
        t1 ++ t2


threeMultiToSingle : Char -> String
threeMultiToSingle c =
    let
        cc =
            c |> toCode

        t1 =
            cc
                |> Bitwise.shiftRightBy 12
                |> Bitwise.or 0xE0
                |> stringify

        t2 =
            cc
                |> Bitwise.shiftRightBy 6
                |> Bitwise.and 0x3F
                |> Bitwise.or 0x80
                |> stringify

        t3 =
            cc
                |> Bitwise.and 0x3F
                |> Bitwise.or 0x80
                |> stringify
    in
        t1 ++ t2 ++ t3


unescape : String -> (Char -> String) -> String -> String
unescape pattern replacement str =
    replace All
        (regex pattern)
        (\{ match } ->
            match
                |> String.toList
                |> List.map replacement
                |> List.foldl (\c a -> a ++ c) ""
        )
        str
