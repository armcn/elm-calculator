module Tests exposing (suite)

import Expect
import Fuzz exposing (..)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        initModel =
            Tuple.first <| init (Flags 1 1)

        updateModel msg model =
            Tuple.first <| update msg model

        button =
            intRange 0 9

        nonZeroButton =
            intRange 1 9

        symbol =
            oneOf
                [ constant Add
                , constant Subtract
                , constant Divide
                , constant Multiply
                , constant Clear
                , constant PlusMinus
                , constant Percentage
                , constant Equals
                , constant Decimal
                ]

        screenState model =
            { display = model.display
            , result = model.result
            }
    in
    describe "Calculator"
        [ describe "Screen state is correct"
            [ test "a number can't start with multiple zeros" <|
                \_ ->
                    initModel
                        |> updateModel (Number 0)
                        |> updateModel (Number 0)
                        |> screenState
                        |> Expect.equal
                            { display = "0"
                            , result = 0
                            }
            , fuzz button "just a number" <|
                \a ->
                    initModel
                        |> updateModel (Number a)
                        |> screenState
                        |> Expect.equal
                            { display = String.fromInt a
                            , result = toFloat a
                            }
            , fuzz symbol "just a symbol" <|
                \a ->
                    initModel
                        |> updateModel a
                        |> screenState
                        |> Expect.equal
                            { display = "0"
                            , result = 0
                            }
            , fuzz2 nonZeroButton button "a multi-digit number" <|
                \a b ->
                    let
                        resultString =
                            String.fromInt a ++ String.fromInt b
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel (Number b)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz nonZeroButton "a number with trailing zeros" <|
                \a ->
                    let
                        resultString =
                            String.fromInt a ++ "00"
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel (Number 0)
                        |> updateModel (Number 0)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz3 button button button "a decimal number can't have multiple decimals" <|
                \a b c ->
                    let
                        resultString =
                            String.concat
                                [ String.fromInt a
                                , "."
                                , String.fromInt b
                                , String.fromInt c
                                ]
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel Decimal
                        |> updateModel (Number b)
                        |> updateModel Decimal
                        |> updateModel (Number c)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz button "clicking decimal button before a number" <|
                \a ->
                    let
                        resultString =
                            "0." ++ String.fromInt a
                    in
                    initModel
                        |> updateModel Decimal
                        |> updateModel (Number a)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz2 button button "a decimal starting with zero" <|
                \a b ->
                    let
                        resultString =
                            String.concat
                                [ "0."
                                , String.fromInt a
                                , String.fromInt b
                                ]
                    in
                    initModel
                        |> updateModel (Number 0)
                        |> updateModel Decimal
                        |> updateModel (Number a)
                        |> updateModel (Number b)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz3 nonZeroButton button button "a decimal starting with a non-zero" <|
                \a b c ->
                    let
                        resultString =
                            String.concat
                                [ String.fromInt a
                                , "."
                                , String.fromInt b
                                , String.fromInt c
                                ]
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel Decimal
                        |> updateModel (Number b)
                        |> updateModel (Number c)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz button "a decimal with trailing zeros" <|
                \a ->
                    let
                        resultString =
                            String.fromInt a ++ ".00"
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel Decimal
                        |> updateModel (Number 0)
                        |> updateModel (Number 0)
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , test "a number and a symbol" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Add
                        |> screenState
                        |> Expect.equal
                            { display = "1 +"
                            , result = 1
                            }
            , test "a number, symbol, and number" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Add
                        |> updateModel (Number 2)
                        |> screenState
                        |> Expect.equal
                            { display = "1 + 2"
                            , result = 3
                            }
            , test "multiple numbers and symbols" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Add
                        |> updateModel (Number 2)
                        |> updateModel Subtract
                        |> updateModel (Number 9)
                        |> screenState
                        |> Expect.equal
                            { display = "1 + 2 - 9"
                            , result = -6
                            }
            , fuzz symbol "symbol before any numbers does nothing" <|
                \a ->
                    initModel
                        |> updateModel a
                        |> screenState
                        |> Expect.equal
                            { display = "0"
                            , result = 0
                            }
            , fuzz button "plus/minus turns positive numbers negative" <|
                \a ->
                    let
                        resultString =
                            String.fromInt (a * -1)
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel PlusMinus
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz button "plus/minus turns negative numbers positive" <|
                \a ->
                    let
                        resultString =
                            String.fromInt a
                    in
                    initModel
                        |> updateModel (Number (a * -1))
                        |> updateModel PlusMinus
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , fuzz button "percentage divides numbers by 100" <|
                \a ->
                    let
                        resultString =
                            String.fromFloat (toFloat a / 100)
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel Percentage
                        |> screenState
                        |> Expect.equal
                            { display = resultString
                            , result = parseFloat resultString
                            }
            , test "equals overwrites inputs with result" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Add
                        |> updateModel (Number 2)
                        |> updateModel Equals
                        |> screenState
                        |> Expect.equal
                            { display = "3"
                            , result = 3
                            }
            , fuzz3 button symbol button "clear returns model to initial state" <|
                \a b c ->
                    initModel
                        |> updateModel (Number a)
                        |> updateModel b
                        |> updateModel (Number c)
                        |> updateModel Clear
                        |> screenState
                        |> Expect.equal (screenState initModel)
            , test "display is less than 11 characters long" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Divide
                        |> updateModel (Number 3)
                        |> updateModel Equals
                        |> .display
                        |> (\a -> String.length a < 11)
                        |> Expect.true "Less than 11 characters"
            ]
        ]
