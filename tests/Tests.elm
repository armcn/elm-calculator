module Tests exposing (suite)

import Expect
import Fuzz exposing (..)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        initModel =
            init (Flags 1 1) |> Tuple.first

        updateModel msg model =
            update msg model |> Tuple.first

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
            { inputs = model.inputs
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
                            { inputs = List.singleton "0"
                            , result = 0
                            }
            , fuzz button "a number" <|
                \a ->
                    initModel
                        |> updateModel (Number a)
                        |> screenState
                        |> Expect.equal
                            { inputs = List.singleton (String.fromInt a)
                            , result = toFloat a
                            }
            , fuzz symbol "a symbol" <|
                \a ->
                    initModel
                        |> updateModel a
                        |> screenState
                        |> Expect.equal
                            { inputs = []
                            , result = 0
                            }
            , fuzz2 nonZeroButton button "a multi-digit number" <|
                \a b ->
                    let
                        resultString =
                            String.append
                                (String.fromInt a)
                                (String.fromInt b)
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel (Number b)
                        |> screenState
                        |> Expect.equal
                            { inputs = List.singleton resultString
                            , result = parseFloat resultString
                            }
            , fuzz nonZeroButton "a number with trailing zeros" <|
                \a ->
                    let
                        resultString =
                            String.append
                                (String.fromInt a)
                                "00"
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel (Number 0)
                        |> updateModel (Number 0)
                        |> screenState
                        |> Expect.equal
                            { inputs = List.singleton resultString
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
                            { inputs = List.singleton resultString
                            , result = parseFloat resultString
                            }
            , fuzz button "clicking decimal button before a number" <|
                \a ->
                    let
                        resultString =
                            String.append "0." (String.fromInt a)
                    in
                    initModel
                        |> updateModel Decimal
                        |> updateModel (Number a)
                        |> screenState
                        |> Expect.equal
                            { inputs = List.singleton resultString
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
                            { inputs = List.singleton resultString
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
                            { inputs = List.singleton resultString
                            , result = parseFloat resultString
                            }
            , fuzz button "a decimal with trailing zeros" <|
                \a ->
                    let
                        resultString =
                            String.append
                                (String.fromInt a)
                                ".00"
                    in
                    initModel
                        |> updateModel (Number a)
                        |> updateModel Decimal
                        |> updateModel (Number 0)
                        |> updateModel (Number 0)
                        |> screenState
                        |> Expect.equal
                            { inputs = List.singleton resultString
                            , result = parseFloat resultString
                            }
            , test "a number and a symbol" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Add
                        |> screenState
                        |> Expect.equal
                            { inputs = [ "1", "+" ]
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
                            { inputs = [ "1", "+", "2" ]
                            , result = 3
                            }
            , test "multiple numbers and symbols" <|
                \_ ->
                    initModel
                        |> updateModel (Number 1)
                        |> updateModel Add
                        |> updateModel (Number 2)
                        |> updateModel Decimal
                        |> updateModel (Number 1)
                        |> updateModel Subtract
                        |> updateModel (Number 0)
                        |> updateModel Decimal
                        |> updateModel (Number 9)
                        |> screenState
                        |> Expect.equal
                            { inputs = [ "1", "+", "2.1", "-", "0.9" ]
                            , result = 2.2
                            }
            , fuzz symbol "symbol before any numbers does nothing" <|
                \a ->
                    initModel
                        |> updateModel a
                        |> screenState
                        |> Expect.equal
                            { inputs = []
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
                            { inputs = List.singleton resultString
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
                            { inputs = List.singleton resultString
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
                            { inputs = List.singleton resultString
                            , result = parseFloat resultString
                            }
            , test "chain of inputs produces correct screen state" <|
                \_ ->
                    initModel
                        |> updateModel (Number 0)
                        |> updateModel (Number 0)
                        |> updateModel Add
                        |> updateModel (Number 2)
                        |> updateModel Decimal
                        |> updateModel (Number 5)
                        |> updateModel PlusMinus
                        |> updateModel Multiply
                        |> updateModel (Number 10)
                        |> updateModel Percentage
                        |> updateModel Subtract
                        |> updateModel (Number 1)
                        |> screenState
                        |> Expect.equal
                            { inputs =
                                [ "0"
                                , "+"
                                , "-2.5"
                                , "x"
                                , "0.1"
                                , "-"
                                , "1"
                                ]
                            , result = -1.25
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
                            { inputs = [ "3" ]
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
            ]
        ]
