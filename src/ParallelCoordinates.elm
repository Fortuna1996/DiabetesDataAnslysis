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
    , firstFunction : LungCancerPrediction -> Float
    , secondFunction : LungCancerPrediction -> Float
    , thirdFunction : LungCancerPrediction -> Float
    , fourthFunction : LungCancerPrediction -> Float
    , firstName : String
    , secondName : String
    , thirdName : String
    , fourthName : String
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
    | ChangeOne (LungCancerPrediction -> Float,String)     
    | ChangeTwo (LungCancerPrediction -> Float,String) 
    | ChangeThree (LungCancerPrediction -> Float,String)
    | ChangeFour (LungCancerPrediction -> Float,String)
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
                    ( Success <| { data = lungCancerPredictionList [ fullText ], firstFunction = .airPollution, secondFunction = .alcoholUse, thirdFunction = .dustAllergy, fourthFunction = .geneticRisk, firstName = "Luftverschmutzung", secondName = "Alkoholkonsum", thirdName = "Stauballergie", fourthName = "genetisches Risiko"}, Cmd.none )

                Err  _ ->
                    ( model, Cmd.none )

        ChangeOne (x, a) ->
            case model of 
                Success m ->
                    ( Success <| { data = m.data, firstFunction = x, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction, firstName = a, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName}, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        ChangeTwo (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFunction, secondFunction = y, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = a, thirdName = m.thirdName, fourthName = m.fourthName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeThree (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = z, fourthFunction = m.fourthFunction, firstName = m.firstName, secondName = m.secondName, thirdName = a, fourthName = m.fourthName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeFour (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = c , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )   
        MoveAxisUp num ->
            case model of
                Success m ->
                    case num of
                        2 ->
                            ( Success <| { m | firstFunction = m.secondFunction, secondFunction = m.firstFunction, firstName = m.secondName, secondName = m.firstName}, Cmd.none )
                        3 ->
                            ( Success <| { m | secondFunction = m.thirdFunction, thirdFunction = m.secondFunction, secondName = m.thirdName, thirdName = m.secondName}, Cmd.none )
                        4 ->
                            ( Success <| { m | thirdFunction = m.fourthFunction, fourthFunction = m.thirdFunction, thirdName = m.fourthName, fourthName = m.thirdName}, Cmd.none )
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
            background: #a255ff3b;
            border: 2px solid #a255ff;
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
            [ Html.Events.onInput (change ChangeOne) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.firstName == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.firstName == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.firstName == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.firstName == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.firstName == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.firstName == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.firstName == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.firstName == "Luftverschmutzung") ]
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
            [ Html.Events.onInput (change ChangeTwo) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.secondName == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.secondName == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.secondName == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.secondName == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.secondName == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.secondName == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.secondName == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.secondName == "Luftverschmutzung") ]
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
            [ Html.Events.onInput (change ChangeThree) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.thirdName == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.thirdName == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.thirdName == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.thirdName == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.thirdName == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.thirdName == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.thirdName == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.thirdName == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
            ]
        ]
    , Html.form
        []
        [ Html.label [] [ Html.text "4. Achse:" ]
        , Html.fieldset
            [ Html.Events.onInput (change ChangeFour) ]
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
                    , Html.Attributes.selected (data.fourthName == "Rauchen") ]
                    [ Html.text "Rauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.fourthName == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.fourthName == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.fourthName == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.fourthName == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.fourthName == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.fourthName == "Stauballergie") ]
                    [ Html.text "Stauballergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.fourthName == "Luftverschmutzung") ]
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
                            multiDimDaten l.data l.firstFunction l.secondFunction l.thirdFunction l.fourthFunction .gender l.firstName l.secondName l.thirdName l.fourthName       
                    in
                    Html.div
                        []
                        [ stylesheet
                        , nav l
                        , parallelCoordinatesPlot 600 2 plotData
                        ]
