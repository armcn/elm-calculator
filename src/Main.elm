module Main exposing (..)

import Browser
import Browser.Events as Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Icons
import Round
import Svg exposing (Svg)
import Svg.Attributes
import VirtualDom



---- MODEL ----


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type alias Button =
    { id : String
    , pressed : Bool
    }


type alias Model =
    { screenSize : ScreenSize
    , button : Button
    , inputs : List String
    , display : String
    , lastValue : String
    , currentValue : String
    , operation : Float -> Float -> Float
    , result : Float
    , append : Bool
    , decimal : Bool
    , cleared : Bool
    }


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screenSize = ScreenSize flags.width flags.height
      , button = Button "" False
      , inputs = []
      , display = "0"
      , lastValue = ""
      , currentValue = "0"
      , operation = defaultOperation
      , result = 0
      , append = False
      , decimal = False
      , cleared = True
      }
    , Cmd.none
    )


defaultOperation : Float -> Float -> Float
defaultOperation _ b =
    b



---- UPDATE ----


type Msg
    = SetScreenSize Int Int
    | PressButton String
    | UnpressButton String
    | Number Int
    | Add
    | Subtract
    | Multiply
    | Divide
    | Percentage
    | PlusMinus
    | Decimal
    | Equals
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SetScreenSize width height ->
            setScreenSize width height model

        PressButton id ->
            pressButton id model

        UnpressButton id ->
            unpressButton id model

        Number number ->
            updateNumbers number model

        Add ->
            add model

        Subtract ->
            subtract model

        Multiply ->
            multiply model

        Divide ->
            divide model

        Decimal ->
            decimal model

        Percentage ->
            percentage model

        PlusMinus ->
            plusMinus model

        Equals ->
            equal model

        Clear ->
            clear model
    , Cmd.none
    )


setScreenSize : Int -> Int -> Model -> Model
setScreenSize width height model =
    { model | screenSize = ScreenSize width height }


pressButton : String -> Model -> Model
pressButton id model =
    { model | button = Button id True }


unpressButton : String -> Model -> Model
unpressButton id model =
    { model | button = Button id False }


updateNumbers : Int -> Model -> Model
updateNumbers number model =
    let
        currentValue =
            newCurrentValue number model

        inputs =
            newInputs currentValue model

        display =
            newDisplay inputs

        result =
            model.operation
                (parseFloat model.lastValue)
                (parseFloat currentValue)
    in
    if displayIsTooLong display then
        model

    else
        { model
            | currentValue = currentValue
            , inputs = inputs
            , display = display
            , result = result
            , append = True
            , decimal = False
            , cleared = False
        }


newCurrentValue : Int -> Model -> String
newCurrentValue number model =
    let
        numberString =
            String.fromInt number

        currentValue =
            model.currentValue
    in
    if model.decimal then
        currentValue ++ "." ++ numberString

    else if model.append && currentValue /= "0" then
        currentValue ++ numberString

    else
        numberString


newInputs : String -> Model -> List String
newInputs currentValue model =
    if model.append || model.decimal then
        replaceLast currentValue model.inputs

    else
        model.inputs ++ List.singleton currentValue


newDisplay : List String -> String
newDisplay inputs =
    case inputs of
        [] ->
            "0"

        _ ->
            inputs
                |> List.intersperse " "
                |> String.concat


displayIsTooLong : String -> Bool
displayIsTooLong display =
    String.length display > 11


replaceLast : a -> List a -> List a
replaceLast replacement list =
    case list of
        [] ->
            list ++ List.singleton replacement

        _ ->
            let
                last =
                    List.length list - 1

                replace index item =
                    if index == last then
                        replacement

                    else
                        item
            in
            List.indexedMap replace list


add : Model -> Model
add =
    updateOperation "+" (+)


subtract : Model -> Model
subtract =
    updateOperation "-" (-)


multiply : Model -> Model
multiply =
    updateOperation "x" (*)


divide : Model -> Model
divide =
    updateOperation "/" (/)


updateOperation : String -> (Float -> Float -> Float) -> Model -> Model
updateOperation id operation model =
    let
        lastValue =
            String.fromFloat model.result

        inputs =
            model.inputs ++ List.singleton id

        display =
            newDisplay inputs

        lastInputWasOperator =
            String.isEmpty model.currentValue
    in
    if
        model.cleared
            || lastInputWasOperator
            || displayIsTooLong display
    then
        model

    else
        { model
            | operation = operation
            , lastValue = lastValue
            , currentValue = ""
            , inputs = inputs
            , display = display
            , append = False
            , decimal = False
        }


decimal : Model -> Model
decimal model =
    if String.contains "." model.currentValue then
        { model | decimal = False }

    else
        { model | decimal = True }


percentage : Model -> Model
percentage =
    modifyCurrentValue (\a -> a / 100)


plusMinus : Model -> Model
plusMinus =
    modifyCurrentValue (\a -> a * -1)


modifyCurrentValue : (Float -> Float) -> Model -> Model
modifyCurrentValue fn model =
    if model.cleared then
        model

    else
        let
            currentValue =
                model.currentValue
                    |> parseFloat
                    |> fn
                    |> String.fromFloat

            inputs =
                replaceLast currentValue model.inputs

            display =
                newDisplay inputs

            result =
                model.operation
                    (parseFloat model.lastValue)
                    (parseFloat currentValue)
        in
        { model
            | currentValue = currentValue
            , inputs = inputs
            , display = display
            , result = result
        }


equal : Model -> Model
equal model =
    if model.cleared then
        model

    else
        let
            resultString =
                roundResult model

            inputs =
                List.singleton resultString
        in
        { model
            | inputs = inputs
            , display = resultString
            , lastValue = ""
            , currentValue = resultString
            , operation = defaultOperation
        }


roundResult : Model -> String
roundResult model =
    let
        rounded =
            Round.round 2 model.result
    in
    if String.contains ".00" rounded then
        String.fromFloat model.result

    else
        rounded


clear : Model -> Model
clear model =
    { model
        | inputs = []
        , display = "0"
        , lastValue = ""
        , currentValue = "0"
        , result = 0
        , operation = defaultOperation
        , append = False
        , cleared = True
    }


parseFloat : String -> Float
parseFloat a =
    Maybe.withDefault 0 (String.toFloat a)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Events.onResize SetScreenSize ]



---- VIEW ----


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , height fill
        , Background.color lightGrey
        ]
    <|
        viewPhone model


viewPhone : Model -> Element Msg
viewPhone model =
    let
        phoneWidth =
            px (calcPhoneWidth model)

        phoneHeight =
            px (calcPhoneHeight model)
    in
    column
        [ width phoneWidth
        , height phoneHeight
        , centerX
        , centerY
        ]
        [ viewScreen model
        , viewButtonPad model
        ]


viewScreen : Model -> Element Msg
viewScreen model =
    let
        cornerRadius =
            calcCornerRadius model
    in
    row
        [ width fill
        , height (fillPortion 1)
        , buttonAreaPadding model
        , Border.roundEach
            { topLeft = cornerRadius
            , topRight = cornerRadius
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Background.color darkBlue
        ]
        [ column
            [ width fill
            , height fill
            ]
            [ viewUpperScreen model
            , viewLowerScreen model
            ]
        ]


viewUpperScreen : Model -> Element Msg
viewUpperScreen model =
    let
        fontSize =
            calcUpperScreenFontSize model
    in
    row [ width fill, height fill ]
        [ el
            [ alignRight
            , alignBottom
            , Font.size fontSize
            , Font.color orange
            ]
          <|
            text model.display
        ]


viewLowerScreen : Model -> Element Msg
viewLowerScreen model =
    let
        screenText =
            if List.length model.inputs <= 2 then
                ""

            else
                roundResult model

        fontSize =
            calcLowerScreenFontSize model
    in
    row [ width fill ]
        [ el
            [ alignRight
            , alignBottom
            , Font.size fontSize
            , Font.color lightGrey
            ]
          <|
            text screenText
        ]


viewButtonPad : Model -> Element Msg
viewButtonPad model =
    let
        cornerRadius =
            calcCornerRadius model

        buttonRow =
            row
                [ width fill
                , height fill
                , spacing (buttonPadSpacing model)
                ]
    in
    row
        [ width fill
        , height (fillPortion 2)
        , buttonAreaPadding model
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = cornerRadius
            , bottomRight = cornerRadius
            }
        , Background.color darkGrey
        ]
        [ column [ width fill, height fill ]
            [ buttonRow
                [ buttonAdd model
                , buttonSubtract model
                , buttonMultiply model
                , buttonDivide model
                ]
            , buttonRow
                [ buttonSeven model
                , buttonEight model
                , buttonNine model
                , buttonClear model
                ]
            , buttonRow
                [ buttonFour model
                , buttonFive model
                , buttonSix model
                , buttonPlusMinus model
                ]
            , buttonRow
                [ buttonOne model
                , buttonTwo model
                , buttonThree model
                , buttonPercent model
                ]
            , buttonRow
                [ buttonZero model
                , buttonDecimal model
                , buttonEquals model
                ]
            ]
        ]


buttonPadSpacing : Model -> Int
buttonPadSpacing model =
    let
        phoneWidth =
            calcPhoneWidth model

        buttonDiameter =
            calcButtonDiameter model

        padding =
            calcButtonAreaPaddingX model

        gridWidth =
            4

        numSpaces =
            gridWidth - 1

        totalSpacing =
            phoneWidth
                - padding
                * 2
                - buttonDiameter
                * gridWidth

        singleSpacing =
            round (toFloat totalSpacing / numSpaces)
    in
    singleSpacing


buttonZero : Model -> Element Msg
buttonZero =
    buttonNumber (Number 0) "0" Icons.zero


buttonOne : Model -> Element Msg
buttonOne =
    buttonNumber (Number 1) "1" Icons.one


buttonTwo : Model -> Element Msg
buttonTwo =
    buttonNumber (Number 2) "2" Icons.two


buttonThree : Model -> Element Msg
buttonThree =
    buttonNumber (Number 3) "3" Icons.three


buttonFour : Model -> Element Msg
buttonFour =
    buttonNumber (Number 4) "4" Icons.four


buttonFive : Model -> Element Msg
buttonFive =
    buttonNumber (Number 5) "5" Icons.five


buttonSix : Model -> Element Msg
buttonSix =
    buttonNumber (Number 6) "6" Icons.six


buttonSeven : Model -> Element Msg
buttonSeven =
    buttonNumber (Number 7) "7" Icons.seven


buttonEight : Model -> Element Msg
buttonEight =
    buttonNumber (Number 8) "8" Icons.eight


buttonNine : Model -> Element Msg
buttonNine =
    buttonNumber (Number 9) "9" Icons.nine


buttonAdd : Model -> Element Msg
buttonAdd =
    buttonSymbolCircle Add "+" Icons.add


buttonSubtract : Model -> Element Msg
buttonSubtract =
    buttonSymbolCircle Subtract "-" Icons.subtract


buttonMultiply : Model -> Element Msg
buttonMultiply =
    buttonSymbolCircle Multiply "x" Icons.multiply


buttonDivide : Model -> Element Msg
buttonDivide =
    buttonSymbolCircle Divide "/" Icons.divide


buttonPlusMinus : Model -> Element Msg
buttonPlusMinus =
    buttonSymbolCircle PlusMinus "+/-" Icons.plusMinus


buttonPercent : Model -> Element Msg
buttonPercent =
    buttonSymbolCircle Percentage "%" Icons.percentage


buttonDecimal : Model -> Element Msg
buttonDecimal =
    buttonSymbolCircle Decimal "." Icons.decimal


buttonEquals : Model -> Element Msg
buttonEquals =
    buttonSymbolPill Equals "=" Icons.equals


buttonClear : Model -> Element Msg
buttonClear =
    buttonSymbolCircle Clear "c" Icons.clear


buttonSymbolCircle :
    Msg
    -> String
    -> (List (VirtualDom.Attribute Msg) -> Svg Msg)
    -> Model
    -> Element Msg
buttonSymbolCircle =
    buttonSymbol 1


buttonSymbolPill :
    Msg
    -> String
    -> (List (VirtualDom.Attribute Msg) -> Svg Msg)
    -> Model
    -> Element Msg
buttonSymbolPill =
    buttonSymbol 2


buttonSymbol :
    Int
    -> Msg
    -> String
    -> (List (VirtualDom.Attribute Msg) -> Svg Msg)
    -> Model
    -> Element Msg
buttonSymbol width msg id icon model =
    let
        buttonPressed =
            model.button.pressed && model.button.id == id

        backgroundColor =
            if buttonPressed then
                black

            else
                orange
    in
    button
        { backgroundColor = backgroundColor
        , fontColor = white
        , width = width
        , msg = msg
        , id = id
        , icon = icon
        }
        model


buttonNumber :
    Msg
    -> String
    -> (List (VirtualDom.Attribute Msg) -> Svg Msg)
    -> Model
    -> Element Msg
buttonNumber msg id icon model =
    let
        buttonPressed =
            model.button.pressed && model.button.id == id

        backgroundColor =
            if buttonPressed then
                black

            else
                lightGrey

        fontColor =
            if buttonPressed then
                white

            else
                darkBlue
    in
    button
        { backgroundColor = backgroundColor
        , fontColor = fontColor
        , width = 1
        , msg = msg
        , id = id
        , icon = icon
        }
        model


type alias ButtonUI =
    { backgroundColor : Color
    , fontColor : Color
    , width : Int
    , msg : Msg
    , id : String
    , icon : List (VirtualDom.Attribute Msg) -> Svg Msg
    }


button : ButtonUI -> Model -> Element Msg
button ui model =
    let
        buttonRadius =
            calcButtonRadius model

        buttonDiameter =
            calcButtonDiameter model

        buttonIconHeight =
            calcIconHeight model

        buttonWidth =
            if ui.width == 1 then
                buttonDiameter

            else
                buttonDiameter
                    * ui.width
                    + buttonPadSpacing model
                    * (ui.width - 1)

        buttonHeight =
            buttonDiameter

        fontColor =
            toSvgColor ui.fontColor
    in
    Input.button
        [ width (px buttonWidth)
        , height (px buttonHeight)
        , centerX
        , centerY
        , Border.rounded buttonRadius
        , Background.color ui.backgroundColor
        , Border.shadow
            { offset = ( 0, 2 )
            , size = 1
            , blur = 1
            , color = black
            }
        , focused []
        , onMouseDown (PressButton ui.id)
        , onMouseUp (UnpressButton ui.id)
        ]
        { onPress = Just ui.msg
        , label =
            el [ centerX, centerY ] <|
                html <|
                    ui.icon
                        [ Svg.Attributes.fill fontColor
                        , Svg.Attributes.height
                            (String.fromInt buttonIconHeight)
                        ]
        }


toSvgColor : Color -> String
toSvgColor color =
    let
        to255 accessor =
            toRgb color
                |> accessor
                |> (*) 255
                |> String.fromFloat
    in
    String.concat
        [ "rgb("
        , to255 .red
        , ","
        , to255 .green
        , ","
        , to255 .blue
        , ")"
        ]


calcPhoneHeight : Model -> Int
calcPhoneHeight model =
    scale 0.9 model.screenSize.height


calcPhoneWidth : Model -> Int
calcPhoneWidth =
    scaleFromHeight 0.46


scaleFromHeight : Float -> Model -> Int
scaleFromHeight factor model =
    scale factor (calcPhoneHeight model)


scale : Float -> Int -> Int
scale factor number =
    number
        |> toFloat
        |> (*) factor
        |> round


calcCornerRadius : Model -> Int
calcCornerRadius =
    scaleFromHeight 0.047


calcButtonRadius : Model -> Int
calcButtonRadius =
    scaleFromHeight 0.038


calcButtonDiameter : Model -> Int
calcButtonDiameter =
    calcButtonRadius >> (*) 2


calcIconHeight : Model -> Int
calcIconHeight =
    calcButtonDiameter >> scale (2 / 3)


calcUpperScreenFontSize : Model -> Int
calcUpperScreenFontSize =
    scaleFromHeight 0.07


calcLowerScreenFontSize : Model -> Int
calcLowerScreenFontSize =
    scaleFromHeight 0.05


buttonAreaPadding : Model -> Attribute msg
buttonAreaPadding model =
    let
        topBottom =
            calcButtonAreaPaddingY model

        leftRight =
            calcButtonAreaPaddingX model
    in
    paddingEach
        { top = topBottom
        , bottom = topBottom
        , left = leftRight
        , right = leftRight
        }


calcButtonAreaPaddingX : Model -> Int
calcButtonAreaPaddingX =
    scaleFromHeight 0.032


calcButtonAreaPaddingY : Model -> Int
calcButtonAreaPaddingY =
    scaleFromHeight 0.076


black : Color
black =
    rgb255 32 34 37


darkGrey : Color
darkGrey =
    rgb255 78 91 116


lightGrey : Color
lightGrey =
    rgb255 229 229 229


white : Color
white =
    rgb255 255 255 255


darkBlue : Color
darkBlue =
    rgb255 28 46 82


orange : Color
orange =
    rgb255 219 144 0


fontFamily : List Font.Font
fontFamily =
    [ Font.external
        { name = "Lato"
        , url = "https://fonts.googleapis.com/css?family=Lato"
        }
    , Font.sansSerif
    ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
