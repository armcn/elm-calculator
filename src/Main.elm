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
import Html.Attributes
import Icons
import Svg exposing (Svg)
import Svg.Attributes
import VirtualDom



---- MODEL ----


type alias Model =
    { screenSize : ScreenSize
    , button : Button
    , inputs : List String
    , lastValue : String
    , currentValue : String
    , result : Float
    , operation : Float -> Float -> Float
    , lastValueNumber : Bool
    , append : Bool
    , decimal : Bool
    , cleared : Bool
    }


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type alias Button =
    { id : String
    , pressed : Bool
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
      , currentValue = "0"
      , lastValue = ""
      , result = 0
      , operation = \_ y -> y
      , lastValueNumber = False
      , append = False
      , decimal = False
      , cleared = True
      }
    , Cmd.none
    )



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
            setScreenSize model width height

        PressButton text ->
            pressButton model text

        UnpressButton text ->
            unpressButton model text

        Number number ->
            updateCurrentValue model number

        Add ->
            addOperation model

        Subtract ->
            subtractOperation model

        Multiply ->
            multiplyOperation model

        Divide ->
            divideOperation model

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


setScreenSize : Model -> Int -> Int -> Model
setScreenSize model width height =
    { model | screenSize = ScreenSize width height }


pressButton : Model -> String -> Model
pressButton model text =
    { model | button = Button text True }


unpressButton : Model -> String -> Model
unpressButton model text =
    { model | button = Button text False }


addOperation : Model -> Model
addOperation =
    inputOperation "+" add


subtractOperation : Model -> Model
subtractOperation =
    inputOperation "-" subtract


multiplyOperation : Model -> Model
multiplyOperation =
    inputOperation "x" multiply


divideOperation : Model -> Model
divideOperation =
    inputOperation "/" divide


inputOperation : String -> (Float -> Float -> Float) -> Model -> Model
inputOperation id operation model =
    if not model.cleared && model.lastValueNumber then
        { model
            | operation = operation
            , currentValue = ""
            , lastValue = String.fromFloat model.result
            , append = False
            , decimal = False
            , lastValueNumber = False
            , inputs = List.append model.inputs [ id ]
        }

    else
        model


updateCurrentValue : Model -> Int -> Model
updateCurrentValue model number =
    let
        newModel =
            if model.append then
                let
                    currentValue =
                        if model.decimal then
                            String.concat
                                [ model.currentValue
                                , "."
                                , String.fromInt number
                                ]

                        else if model.currentValue == "0" then
                            "0"

                        else
                            String.append
                                model.currentValue
                                (String.fromInt number)
                in
                { model
                    | currentValue = currentValue
                    , decimal = False
                    , cleared = False
                    , inputs = replaceLast currentValue model.inputs
                }

            else if model.lastValueNumber then
                let
                    currentValue =
                        String.fromInt number
                in
                { model
                    | currentValue = currentValue
                    , cleared = False
                    , append = True
                    , inputs = replaceLast currentValue model.inputs
                }

            else
                { model
                    | currentValue = String.fromInt number
                    , append = True
                    , cleared = False
                    , inputs =
                        List.append model.inputs
                            [ String.fromInt number ]
                }

        result =
            calculate newModel
    in
    { newModel
        | result = result
        , lastValueNumber = True
    }


replaceLast : a -> List a -> List a
replaceLast val list =
    case list of
        [] ->
            List.append list [ val ]

        _ ->
            list
                |> List.indexedMap
                    (\index item ->
                        if index == (List.length list - 1) then
                            val

                        else
                            item
                    )


percentage : Model -> Model
percentage model =
    if model.cleared then
        model

    else
        let
            currentValue =
                parseFloat model.currentValue
                    / 100
                    |> String.fromFloat

            newModel =
                { model
                    | currentValue = currentValue
                    , inputs = replaceLast currentValue model.inputs
                }
        in
        { newModel | result = calculate newModel }


plusMinus : Model -> Model
plusMinus model =
    if model.cleared then
        model

    else
        let
            currentValue =
                parseFloat model.currentValue
                    * -1
                    |> String.fromFloat

            newModel =
                { model
                    | currentValue = currentValue
                    , inputs = replaceLast currentValue model.inputs
                }
        in
        { newModel | result = calculate newModel }


decimal : Model -> Model
decimal model =
    if String.contains "." model.currentValue then
        model

    else
        { model
            | decimal = True
            , append = True
        }


equal : Model -> Model
equal model =
    if model.cleared then
        model

    else
        { model
            | inputs = [ String.fromFloat model.result ]
            , currentValue = String.fromFloat model.result
            , lastValue = ""
            , result = model.result
            , operation = \_ y -> y
            , append = False
        }


clear : Model -> Model
clear model =
    { model
        | inputs = []
        , currentValue = "0"
        , lastValue = ""
        , result = 0
        , operation = \_ y -> y
        , append = False
        , cleared = True
    }


calculate : Model -> Float
calculate model =
    model.operation
        (parseFloat model.lastValue)
        (parseFloat model.currentValue)


parseFloat : String -> Float
parseFloat input =
    Maybe.withDefault 0 (String.toFloat input)


add : Float -> Float -> Float
add x y =
    x + y


subtract : Float -> Float -> Float
subtract x y =
    x - y


multiply : Float -> Float -> Float
multiply x y =
    x * y


divide : Float -> Float -> Float
divide x y =
    x / y



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\values -> SetScreenSize values)
        ]



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
            px <| calcPhoneWidth model

        phoneHeight =
            px <| calcPhoneHeight model
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
        , height <| fillPortion 1
        , buttonPadPadding model
        , Background.color darkBlue
        , Border.roundEach
            { topLeft = cornerRadius
            , topRight = cornerRadius
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]
        [ column [ width fill, height fill ]
            [ upperScreen model
            , lowerScreen model
            ]
        ]


upperScreen : Model -> Element Msg
upperScreen model =
    let
        screenText =
            case model.inputs of
                [] ->
                    "0"

                _ ->
                    model.inputs
                        |> List.intersperse " "
                        |> String.concat
    in
    row [ width fill, height fill ]
        [ el
            [ alignRight
            , alignBottom
            , Font.color orange
            , Font.size 50
            ]
          <|
            text screenText
        ]


lowerScreen : Model -> Element Msg
lowerScreen model =
    let
        screenText =
            if List.length model.inputs <= 2 then
                ""

            else
                String.fromFloat model.result
    in
    row [ width fill ]
        [ el
            [ alignRight
            , alignBottom
            , Font.color lightGrey
            , Font.size 30
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
                , spaceEvenly
                ]
    in
    row
        [ width fill
        , height <| fillPortion 2
        , buttonPadPadding model
        , Background.color darkGrey
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = cornerRadius
            , bottomRight = cornerRadius
            }
        ]
        [ column [ width fill, height fill ]
            [ buttonRow
                [ buttonAdd model
                , buttonSubtract model
                , buttonMultiply model
                , buttonDivide model
                ]
            , buttonRow
                [ buttonNumber (Number 7) "7" Icons.seven model
                , buttonNumber (Number 8) "8" Icons.eight model
                , buttonNumber (Number 9) "9" Icons.nine model
                , buttonClear model
                ]
            , buttonRow
                [ buttonNumber (Number 4) "4" Icons.four model
                , buttonNumber (Number 5) "5" Icons.five model
                , buttonNumber (Number 6) "6" Icons.six model
                , buttonPlusMinus model
                ]
            , buttonRow
                [ buttonNumber (Number 1) "1" Icons.one model
                , buttonNumber (Number 2) "2" Icons.two model
                , buttonNumber (Number 3) "3" Icons.three model
                , buttonPercent model
                ]
            , buttonRow
                [ buttonNumber (Number 0) "0" Icons.zero model
                , buttonDecimal model
                , buttonEquals model
                ]
            ]
        ]


buttonAdd =
    buttonSymbolCircle Add "+" Icons.add


buttonSubtract =
    buttonSymbolCircle Subtract "-" Icons.subtract


buttonMultiply =
    buttonSymbolCircle Multiply "x" Icons.multiply


buttonDivide =
    buttonSymbolCircle Divide "/" Icons.divide


buttonPlusMinus =
    buttonSymbolCircle PlusMinus "+/-" Icons.plusMinus


buttonPercent =
    buttonSymbolCircle Percentage "%" Icons.percentage


buttonDecimal =
    buttonSymbolCircle Decimal "." Icons.decimal


buttonEquals =
    buttonSymbolPill Equals "=" Icons.equals


buttonClear =
    buttonSymbolCircle Clear "c" Icons.clear



--buttonSymbolCircle : Msg -> String -> Svg Msg -> Model -> Element Msg


buttonSymbolCircle =
    buttonSymbol 1



--buttonSymbolPill : Msg -> String -> Svg Msg -> Model -> Element Msg


buttonSymbolPill =
    buttonSymbol 2



--buttonSymbol : Int -> Msg -> String -> Svg Msg -> Model -> Element Msg


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



--buttonNumber : Msg -> String -> Svg Msg -> Model -> Element Msg


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
            buttonRadius * 2

        buttonIconHeight =
            calcIconHeight model

        buttonWidth =
            if ui.width == 1 then
                px <| buttonDiameter

            else
                fill

        buttonHeight =
            px buttonDiameter
    in
    column
        [ width <| fillPortion ui.width
        , height buttonHeight
        ]
        [ Input.button
            [ width buttonWidth
            , height buttonHeight
            , centerX
            , centerY
            , Background.color ui.backgroundColor
            , Border.rounded buttonRadius
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 1
                , blur = 1
                , color = black
                }
            , focused []
            , onMouseDown <| PressButton ui.id
            , onMouseUp <| UnpressButton ui.id
            ]
            { onPress = Just ui.msg
            , label =
                ui.icon
                    [ Svg.Attributes.fill (toSvgColor ui.fontColor)
                    , Svg.Attributes.height (String.fromInt buttonIconHeight)
                    ]
                    |> html
                    |> el [ centerX, centerY ]
            }
        ]


toSvgColor : Color -> String
toSvgColor color =
    let
        rgbRecord =
            toRgb color

        redVal =
            String.fromFloat <|
                rgbRecord.red
                    * 255

        greenVal =
            String.fromFloat <|
                rgbRecord.green
                    * 255

        blueVal =
            String.fromFloat <|
                rgbRecord.blue
                    * 255
    in
    "rgb("
        ++ redVal
        ++ ","
        ++ greenVal
        ++ ","
        ++ blueVal
        ++ ")"


calcPhoneHeight : Model -> Int
calcPhoneHeight model =
    model.screenSize.height
        |> toFloat
        |> (*) 0.9
        |> round


scaleFromHeight : Float -> Model -> Int
scaleFromHeight scale model =
    calcPhoneHeight model
        |> toFloat
        |> (*) scale
        |> round


calcPhoneWidth : Model -> Int
calcPhoneWidth =
    scaleFromHeight 0.46


calcCornerRadius : Model -> Int
calcCornerRadius =
    scaleFromHeight 0.047


calcButtonRadius : Model -> Int
calcButtonRadius =
    scaleFromHeight 0.041


calcIconHeight : Model -> Int
calcIconHeight =
    calcButtonRadius


buttonPadPadding : Model -> Attribute msg
buttonPadPadding model =
    let
        topBottom =
            scaleFromHeight 0.059 model

        leftRight =
            scaleFromHeight 0.023 model
    in
    paddingEach
        { top = topBottom
        , bottom = topBottom
        , left = leftRight
        , right = leftRight
        }


black : Color
black =
    rgb255 32 34 37


darkGrey : Color
darkGrey =
    rgb255 78 91 116


darkBlue : Color
darkBlue =
    rgb255 28 46 82


lightGrey : Color
lightGrey =
    rgb255 229 229 229


orange : Color
orange =
    rgb255 219 144 0


white : Color
white =
    rgb255 255 255 255


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
