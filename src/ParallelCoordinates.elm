module ParallelCoordinates exposing (..)
import Axis
import Browser
import Color
import Csv.Decode
import TypedSvg exposing (path)
import Html exposing (Html, a, li, ul)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape 
import Statistics 
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox, class)
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Tuple exposing (second)
import Csv exposing (Csv)
import Html.Attributes
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Attributes



type alias Data =
    { data : List LungCancerPrediction
    , firstFUNCTION : LungCancerPrediction -> Float
    , secondFUNCTION : LungCancerPrediction -> Float
    , thirdFUNCTION : LungCancerPrediction -> Float
    , fourthFUNCTION : LungCancerPrediction -> Float
    , firstNAME : String
    , secondNAME : String
    , thirdNAME : String
    , fourthNAME : String
    }

type Model
  = Error
  | Loading
  | Success Data

type alias LungCancerPrediction =
    { gender : String
    , index : Float
    , age : Float
    , airPollution : Float
    , alcoholUse : Float
    , dustAllergy : Float
    , geneticRisk : Float
    , obesity : Float
    , smoking : Float
    }

type alias MultiDimData = 
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }

type Gender
    = M
    | F
    | UnknownGender

type alias MultiDimPoint =
    { pointName : String, gender: Gender, value : List Float }

main : Program () Model Msg
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getCsv GotText
    )

getCsv : (Result Http.Error String -> Msg) -> Cmd Msg
getCsv x = 
    list
        |> List.map
            (\data ->
                Http.get
                    { url = "https://raw.githubusercontent.com/Fortuna1996/LungCancerPrediction/main/" ++ data
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

list : List String 
list = 
    [ "cancer%20patient%20data%20sets%20update2.csv" ]


type Msg 
    = GotText (Result Http.Error String)   
    | ChangeONE (LungCancerPrediction -> Float,String)     
    | ChangeTWO (LungCancerPrediction -> Float,String) 
    | ChangeTHREE (LungCancerPrediction -> Float,String)
    | ChangeFOUR (LungCancerPrediction -> Float,String)
    | MoveAxisUp Int
    
csvStringToData : String -> List LungCancerPrediction
csvStringToData csvR = 
    Csv.parse csvR
        |> Csv.Decode.decodeCsv decodingLungCancerPrediction
        |> Result.toMaybe
        |> Maybe.withDefault []

decodingLungCancerPrediction : Csv.Decode.Decoder (LungCancerPrediction -> a) a
decodingLungCancerPrediction =
    Csv.Decode.map LungCancerPrediction
        (Csv.Decode.field "gender" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "index"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "age"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "airPollution"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "alcoholUse"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "dustAllergy"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "geneticRisk"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "obesity"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "smoking"(String.toFloat >> Result.fromMaybe "error parsing string"))
            )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = lungCancerPredictionList [ fullText ], firstFUNCTION = .airPollution, secondFUNCTION = .alcoholUse, thirdFUNCTION = .dustAllergy, fourthFUNCTION = .geneticRisk, firstNAME = "Luftverschmutzung", secondNAME = "Alkoholkonsum", thirdNAME = "Stauballergie", fourthNAME = "genetisches Risiko"}, Cmd.none )

                Err  _ ->
                    ( model, Cmd.none )

        ChangeONE (x, a) ->
            case model of 
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = x, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = m.fourthFUNCTION, firstNAME = a, secondNAME = m.secondNAME, thirdNAME = m.thirdNAME, fourthNAME = m.fourthNAME}, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        ChangeTWO (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = y, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = m.fourthFUNCTION , firstNAME = m.firstNAME, secondNAME = a, thirdNAME = m.thirdNAME, fourthNAME = m.fourthNAME}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeTHREE (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = z, fourthFUNCTION = m.fourthFUNCTION, firstNAME = m.firstNAME, secondNAME = m.secondNAME, thirdNAME = a, fourthNAME = m.fourthNAME}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeFOUR (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = c , firstNAME = m.firstNAME, secondNAME = m.secondNAME, thirdNAME = m.thirdNAME, fourthNAME = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )   
        MoveAxisUp num ->
            case model of
                Success m ->
                    case num of
                        2 ->
                            ( Success <| { m | firstFUNCTION = m.secondFUNCTION, secondFUNCTION = m.firstFUNCTION, firstNAME = m.secondNAME, secondNAME = m.firstNAME}, Cmd.none )
                        3 ->
                            ( Success <| { m | secondFUNCTION = m.thirdFUNCTION, thirdFUNCTION = m.secondFUNCTION, secondNAME = m.thirdNAME, thirdNAME = m.secondNAME}, Cmd.none )
                        4 ->
                            ( Success <| { m | thirdFUNCTION = m.fourthFUNCTION, fourthFUNCTION = m.thirdFUNCTION, thirdNAME = m.fourthNAME, fourthNAME = m.thirdNAME}, Cmd.none )
                        _ ->
                            ( model, Cmd.none )     
                _ ->
                    ( model, Cmd.none )     

lungCancerPredictionList :List String -> List LungCancerPrediction    
lungCancerPredictionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat            

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

padding : Float
padding =
    60

radius : Float
radius =
    5.0

tickCount : Int
tickCount =
    8

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extent =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extent |> max 0
    , Tuple.second closeExtent + extent
    )

parallelCoordinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoordinatesPlot w ar model =
    let
        h : Float
        h =
            w / ar

        listTransformieren : List (List Float)
        listTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listWideExtent : List ( Float, Float )
        listWideExtent =
            listTransformieren |> List.map wideExtent

        listScale =
            List.map (Scale.linear ( h, 0 )) listWideExtent

        listAxis =
            List.map (Axis.left [ Axis.tickCount tickCount ]) listScale

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 3 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style
            []
            [ TypedSvg.Core.text
                """
                .parallelPoint path {
                    stroke: #dddddd;
                    stroke-width: 2;
                    stroke-opacity: 0.1;
                    transition: stroke 0.1s ease;
                }

                .parallelPoint.gender-male path {
                    stroke: #55bfff;
                }

                .parallelPoint.gender-female path {
                    stroke: #ff455f;
                }

                .parallelPoint:hover path {
                    stroke-opacity: 1;
                }

                .parallelPoint text {
                    font-family: "Inter Tight", sans-serif;
                    font-size: small;
                    fill: #000;
                    stroke: none;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.15s ease;
                }

                .parallelPoint:hover text {
                    visibility: visible;
                    opacity: 1;
                }  
                """
            ]
        , g []
            [ g [ transform [ Translate (padding*2)  padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
            , g [ transform [ Translate (padding*2) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "Times New Roman" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert xScale (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p name gender description =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listScale
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        g [class 
                            [ "parallelPoint"
                            , case gender of
                                M -> "gender-male"
                                F -> "gender-female"
                                UnknownGender -> "gender-unknown"
                            ]
                          ][
                            Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            ]
                            , text_
                                [ x 300
                                , y -50
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                , fontSize <| Px 2
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p description)))]
                                
                        ]
                        
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding*2) padding ] ]
                                (List.map (\a -> drawPoint a.value a.pointName a.gender model.dimDescription) dataset)
                        )
               )


stylesheet : Html.Html Msg
stylesheet =
  let
    styles = 
        """
        #parallel-nav {
            display: flex;
            flex-direction: column;
            gap: 1em;
            margin: -1.5em -1em 1em -1em;
            padding: 1em; 
            padding-top: 1.5em; 
            background: #f8f8f8;
            border-bottom: 1px solid #dddddd;
        }

        #parallel-nav > span {
            flex: 0 0 100%;
        }

        #parallel-nav > form {
            display: flex;
            gap: 3em;
        }

        #parallel-nav > form > fieldset {
            display: flex;
            gap: 0.5em;
            border: none;
            padding: 0;
        }

        #parallel-nav > form > fieldset > button {
            border-radius: 50%;
            background: #55bfff3b;
            border: 2px solid #55bfff;
            transition: background 0.2s ease;
        }

        #parallel-nav > form > fieldset > button[disabled] {
            opacity: 0.2;
        }

        #parallel-nav > form > fieldset > button:hover {
            background: #55bfff;
        }

        #parallel-nav > form > label {
            flex: 0 8%;
        }

        #parallel-nav > form > select {
            flex: 1;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]

change : ((LungCancerPrediction -> Float, String) -> Msg) -> String -> Msg
change msg value =
    case value of
        "Patientennummer" -> msg (.index, "Patientennummer")
        "Alter des Patienten" -> msg (.age, "Alter des Patienten")
        "Luftverschmutzung" -> msg (.airPollution, "Luftverschmutzung")
        "Alkoholkonsum" -> msg (.alcoholUse, "Alkoholkonsum")
        "Stauballergie" -> msg (.dustAllergy, "Stauballergie")
        "genetisches Risiko" -> msg (.geneticRisk, "genetisches Risiko")
        "Adipositas" -> msg (.obesity, "Adipositas")
        "Rauchen" -> msg (.smoking, "Rauchen")
        _ -> msg (.obesity, "Adipositas")


nav : Data -> Html Msg
nav data = Html.nav
    [ Html.Attributes.id "parallel-nav" ]
    [ Html.span [] [ Html.text "Wechseln Sie die 4 Achsen, um verschiedene Zusammenhänge in den Parallelen Koordinaten zu erkunden." ]
    , Html.form
        []
        [ Html.label [] [ Html.text "1. Achse:" ]
        , Html.fieldset
            []
            [ Html.button
                [ Html.Attributes.disabled True
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ onClick (MoveAxisUp 2)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            [ Html.Events.onInput (change ChangeONE) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.firstNAME == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.firstNAME == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.firstNAME == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.firstNAME == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.firstNAME == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.firstNAME == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
        ]
    ]
    , Html.form
        []
        [ Html.label [] [ Html.text "2. Achse:" ]
        , Html.fieldset
            []
            [ Html.button
                [ onClick (MoveAxisUp 2)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ onClick (MoveAxisUp 3)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            [ Html.Events.onInput (change ChangeTWO) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.firstNAME == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.firstNAME == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.firstNAME == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.firstNAME == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.firstNAME == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.firstNAME == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
            ]
        ]
    , Html.form
        []
        [ Html.label [] [ Html.text "3. Achse:" ]
        , Html.fieldset
            []
            [ Html.button
                [ onClick (MoveAxisUp 3)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ onClick (MoveAxisUp 4)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            [ Html.Events.onInput (change ChangeTHREE) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.firstNAME == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.firstNAME == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.firstNAME == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.firstNAME == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.firstNAME == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.firstNAME == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
            ]
        ]
    , Html.form
        []
        [ Html.label [] [ Html.text "4. Achse:" ]
        , Html.fieldset
            [ Html.Events.onInput (change ChangeFOUR) ]
            [ Html.button
                [ onClick (MoveAxisUp 4)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ Html.Attributes.disabled True
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            []
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.firstNAME == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.firstNAME == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.firstNAME == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.firstNAME == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.firstNAME == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.firstNAME == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.firstNAME == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
            ]
        ]
    ]

genderLabel : String -> String
genderLabel gender = case gender of 
    "M" -> "männlich"
    "F" -> "weiblich"
    _ -> "unbekannt"

genderFlag :  String -> Gender
genderFlag gender = case gender of 
    "M" -> M
    "F" -> F
    _ -> UnknownGender
               
view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Leider konnten die Parallelen Koordinaten der Lungenkrebspatienten bezüglich ihrer Attribute nicht geladen werden."

        Loading ->
            Html.span
                []
                [ Html.text "Lade Parallelen Koordinaten der Lungenkrebspatienten... "
                , FontAwesome.view (FontAwesome.styled [ FontAwesome.Attributes.spin ] FontAwesome.Solid.spinner)
                ]

        Success l ->
                    let
                        multiDimDaten : List LungCancerPrediction -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> String) -> String -> String -> String -> String -> MultiDimData
                        multiDimDaten listLungCancerPrediction a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPoint (genderLabel (e x)) (genderFlag (e x))
                                )
                                listLungCancerPrediction
                            ]

                        plotData = 
                            multiDimDaten l.data l.firstFUNCTION l.secondFUNCTION l.thirdFUNCTION l.fourthFUNCTION .gender l.firstNAME l.secondNAME l.thirdNAME l.fourthNAME       
                    in
                    Html.div
                        []
                        [ stylesheet
                        , nav l
                        , parallelCoordinatesPlot 600 2 plotData
                        ]
