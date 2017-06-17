module Test.Bytes exposing (tests)

import Array as Array
import Expect exposing (..)
import Test exposing (..)
import Bytes
    exposing
        ( Bytes
        , empty
        , fromList
        , fromBytes
        , fromUTF8
        , fromHex
        , isBytes
        , isEmpty
        , isHex
        , length
        , toArray
        , toList
        , toString
        )


tests : Test
tests =
    describe "Tests for Bytes type representation in Elm"
        [ test "Empty `Bytes` container" <|
            \() ->
                Bytes.empty
                    |> Bytes.length
                    |> Expect.equal 0
        , test "`Bytes` from `List` of `Int` with valid values" <|
            \() ->
                Bytes.fromList [ 255 ]
                    |> Result.withDefault empty
                    |> Bytes.length
                    |> Expect.equal 1
        , test "`Bytes` from `List` of `Int` with invalid values" <|
            \() ->
                Bytes.fromList [ 256 ]
                    |> Expect.equal
                        (Err "Invalid numbers! Numbers must be between 0 and 255")
        , test "Converts a `String`, unescape(encodeURI(..)), to `Bytes`" <|
            \() ->
                Bytes.fromBytes "Ã¦ Ã¸ Ã¥ Ã±"
                    |> Expect.equal
                        (Bytes.fromList
                            [ 195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177 ]
                            |> Result.withDefault empty
                        )
        , test "Converts a `Hex` `String` to `Bytes`" <|
            \() ->
                Bytes.fromHex "7FFF"
                    |> Expect.equal
                        (Bytes.fromList
                            [ 127, 255 ]
                            |> Result.withDefault empty
                        )
        , test "Converts an `URI` `String` to `Bytes`" <|
            \() ->
                Bytes.fromURI "%C3%A6%20%C3%B8%20%C3%A5%20%C3%B1"
                    |> Expect.equal
                        (Bytes.fromList
                            [ 195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177 ]
                            |> Result.withDefault empty
                        )
        , test "Converts an `UTF-8` `String` to `Bytes`" <|
            \() ->
                Bytes.fromUTF8 "æ ø å ñ"
                    |> Expect.equal
                        (Bytes.fromList
                            [ 195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177 ]
                            |> Result.withDefault empty
                        )
        , test "Determine if each `Char` in a `String` represents a single `Byte`" <|
            \() ->
                Bytes.isBytes "Ã¦ Ã¸ Ã¥ Ã±"
                    |> Expect.equal True
        , test "Determine if the `Bytes` container is empty" <|
            \() ->
                Bytes.isEmpty Bytes.empty
                    |> Expect.equal True
        , test "Determine if all `Char` in a `String` are `HexDigits`" <|
            \() ->
                Bytes.isHex "7FFF"
                    |> Expect.equal True
        , test "The number of the `Bytes`" <|
            \() ->
                Bytes.fromList
                    [ 127, 255 ]
                    |> Result.withDefault empty
                    |> Bytes.length
                    |> Expect.equal 2
        , test "Creates an `Array` of `Int` from `Bytes`" <|
            \() ->
                Bytes.fromList
                    [ 127, 255 ]
                    |> Result.withDefault empty
                    |> Bytes.toArray
                    |> Expect.equal ([ 127, 255 ] |> Array.fromList)
        , test "Creates an `List` of `Int` from `Bytes`" <|
            \() ->
                Bytes.fromList
                    [ 127, 255 ]
                    |> Result.withDefault empty
                    |> Bytes.toList
                    |> Expect.equal [ 127, 255 ]
        , test "Creates a `String`, unescape(encodeURI(..)), from `Bytes`" <|
            \() ->
                Bytes.fromList
                    [ 195, 166, 32, 195, 184, 32, 195, 165, 32, 195, 177 ]
                    |> Result.withDefault empty
                    |> Bytes.toString
                    |> Expect.equal "Ã¦ Ã¸ Ã¥ Ã±"
        ]
