module Scatterplot exposing (..)

import Axis
import Html exposing (Html)
import Http
import Scale exposing (ContinuousScale)
import Statistics 
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment (..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import Html exposing (li)
import Html.Events exposing (onClick)
import Html exposing (ul)
import Html.Attributes
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Attributes

import Patients exposing (..)

type alias Data =
    { data : List LungCancerPrediction
    , xFunction : LungCancerPrediction -> Float
    , yFunction : LungCancerPrediction -> Float
    , xName : String 
    , yName : String
    , chosendata : Maybe LungCancerPrediction
    }

type Model
 = Error
 | Loading
 | Success Data


type alias LungCancerPrediction =
    { gender : Gender
    , index : Float
    , age : Float
    , airPollution : Float
    , alcoholUse : Float
    , dustAllergy : Float
    , geneticRisk : Float
    , obesity: Float
    , smoking : Float
    , passiveSmoker : Float
    , chronicLungDisease : Float
    , balancedDiet : Float
    , chestPain : Float
    }
type Msg
    = GotText (Result Http.Error String)
    | ChangeX (LungCancerPrediction -> Float, String)
    | ChangeY (LungCancerPrediction -> Float, String)
    | PointChosen LungCancerPrediction



type alias Point = 
    { pointName : String, x : Float, y : Float, gender: Gender}
type alias XYData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    , chosendata : Maybe Point
    }

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

csvStringToData : String -> List LungCancerPrediction
csvStringToData csvR =
    Csv.parse csvR
        |> Csv.Decode.decodeCsv decodingLungCancerPrediction
        |> Result.toMaybe
        |>Maybe.withDefault []

decodingLungCancerPrediction : Csv.Decode.Decoder (LungCancerPrediction -> a) a
decodingLungCancerPrediction =
        Csv.Decode.map LungCancerPrediction
            (Csv.Decode.field "gender" (\s -> Result.fromMaybe "gender not ok" (Just (genderFlag s)))
                
                |> Csv.Decode.andMap (Csv.Decode.field "index"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "age"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "airPollution"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "alcoholUse"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "dustAllergy"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "geneticRisk"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "obesity"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "smoking"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "passiveSmoker"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "chronicLungDisease"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "balancedDiet"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "chestPain"(String.toFloat >> Result.fromMaybe "error parsing string"))

            )
            -- hinzufügen update : Msg

        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = lungCancerPredictionList [ fullText ], xFunction = .airPollution, yFunction = .alcoholUse, xName = "Luftverschmutzung", yName = "Alkoholkonsum", chosendata = Nothing}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = x, yFunction = m.yFunction, xName = a, yName = m.yName , chosendata = m.chosendata}, Cmd.none )
                    --( Success <| {m.data| xFunction=x, xName=a}, Cmd.none)

                _ ->
                    ( model, Cmd.none )
        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = m.xFunction, yFunction = y, xName = m.xName, yName = a, chosendata = m.chosendata }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        PointChosen sac ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = m.xFunction, yFunction = m.yFunction, xName = m.xName, yName = m.yName, chosendata = Just sac }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



lungCancerPredictionList :List String -> List LungCancerPrediction
lungCancerPredictionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

{- filterReducedLungCancerPrediction : List LungCancerPrediction -> Maybe LungCancerPrediction -> (LungCancerPrediction -> String) -> (LungCancerPrediction -> Float) -> (LungCancerPrediction->Float) -> String -> String -> XYData 

filterReducedLungCancerPrediction lungCancerPredictionsliste mchosen a b c x y =
    XYData x y (List.map (\n -> pointName n a b c x y) lungCancerPredictionsliste) (Maybe.map (\n -> pointName n a b c x y ) mchosen)
 -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- nach vorne 
w : Float
w =
    900

h : Float
h =
    450

padding : Float
padding =
    60

radius : Float
radius =
    5.0

tickCount : Int
tickCount =
    5

xAsis : List Float -> Svg msg
xAsis values = 
    Axis.bottom [ Axis.tickCount tickCount] (xScale values)

yAxis : List Float -> Svg msg 
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] ( yScale values )

xScale : List Float -> ContinuousScale Float 
xScale values = 
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )

yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

adding : (Float, Float) -> Float-> (Float, Float) 
adding (min, max) x =
    if min <= 0 then
        ( 0, max + x)
    else 
        (min - x, max + x)

wideExtent : List Float -> ( Float, Float )
wideExtent values = 
    let
        result = 
            Maybe.withDefault (0, 0)
            (Statistics.extent values)
        
        max =          
            Maybe.withDefault (0)
            (List.maximum values)
            
        result1 = 
            adding result (toFloat(tickCount)*max/50)
        
        result2 = 
            adding result1 (0.0)       
    in
        result2

genderLabel : Gender -> String
genderLabel gender =
    case gender of
        M ->
            "männlich"

        F ->
            "weiblich"

        _ ->
            "unbekannt"


genderFlag : String -> Gender
genderFlag gender =
    case gender of
        "M" ->
            M

        "F" ->
            F

        _ ->
            UnknownGender



{- pointName : LungCancerPrediction -> (LungCancerPrediction -> String) -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> Float) -> String -> String -> Point
   pointName lungCancerPrediction u v x y z =
       Point (genderLabel (u lungCancerPrediction) ++ ", " ++ y ++ ": " ++ String.fromFloat (v lungCancerPrediction) ++ ", " ++ z ++ ": " ++ String.fromFloat (x lungCancerPrediction)) (v lungCancerPrediction) (x lungCancerPrediction) (genderFlag (u lungCancerPrediction))
-}


point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY yxPoint =
    g
        [ class
            [ "point"
            , case yxPoint.gender of
                M ->
                    "gender-male"

                F ->
                    "gender-female"

                UnknownGender ->
                    "gender-unknown"
            ]
        , fontSize <| Px 15.0

        {- ,transform
           [
               Translate
               --(Scale.convert scaleX yxPoint.x)
               --(Scale.convert scaleY yxPoint.y)
           ]
        -}
        ]
        [ circle [ cx (Scale.convert scaleX yxPoint.x), cy (Scale.convert scaleY yxPoint.y), r 5 ] []
        , text_ [ x ((w / 4) + 0), y -20 ] [ Html.text yxPoint.pointName ]
        ]


drawpoint : ContinuousScale Float -> ContinuousScale Float -> (LungCancerPrediction -> Float) -> String -> (LungCancerPrediction -> Float) -> String -> LungCancerPrediction -> Svg Msg
drawpoint scaleX scaleY xfunc xname yfunc yname sac =
    g
        [ class
            [ "point"
            , case sac.gender of
                M ->
                    "gender-male"

                F ->
                    "gender-female"

                UnknownGender ->
                    "gender-unknown"
            ]
        , fontSize <| Px 15.0

        {- ,transform
           [
               Translate
               (Scale.convert scaleX (xfunc sac))
               (Scale.convert scaleY (yfunc sac))
           ]
        -}
        ]
        [ circle [ cx (Scale.convert scaleX (xfunc sac)), cy (Scale.convert scaleY (yfunc sac)), r 5 ] []
        , text_ [ x ((w / 4) + 0), y -20 ] [ Html.text (genderLabel sac.gender ++ ", " ++ xname ++ ": " ++ String.fromFloat (xfunc sac) ++ ", " ++ yname ++ ": " ++ String.fromFloat (yfunc sac)) ]
        ]


drawChosenpoint : ContinuousScale Float -> ContinuousScale Float -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> Float) -> Maybe LungCancerPrediction -> String -> String -> List (Svg Msg)
drawChosenpoint scaleX scaleY xfunc yfunc msac xname yname =
    case msac of
        Nothing ->
            []

        Just sac ->
            [ g
                [ class
                    [ "cpoint"
                    , case sac.gender of
                        M ->
                            "gender-male"

                        F ->
                            "gender-female"

                        UnknownGender ->
                            "gender-unknown"
                    ]
                , fontSize <| Px 15.0

                {- ,transform
                   [
                       Translate
                       (Scale.convert scaleX (xfunc sac))
                       (Scale.convert scaleY (yfunc sac))
                   ]
                -}
                ]
                [ circle [ cx (Scale.convert scaleX (xfunc sac)), cy (Scale.convert scaleY (yfunc sac)), r 5 ] []
                , text_ [ x ((w / 4) + 0), y -20 ] [] --[ Html.text (genderLabel sac.gender ++ ", " ++ xname ++ ": " ++ String.fromFloat (xfunc sac) ++ ", " ++ yname ++ ": " ++ String.fromFloat (yfunc sac)) ]
                ]
            ]



scatterplot : Data -> Svg Msg
scatterplot model =
    let
        xValues : List Float
        xValues =
            List.map model.xFunction model.data

        yValues : List Float
        yValues =
            List.map model.yFunction model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPosition : { x : Float, y : Float }
        labelPosition =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style
            []
            [ TypedSvg.Core.text
                """
                .point circle {
                    stroke: #dddddd;
                    fill: #dddddd;
                    stroke-width: 2;
                    stroke-opacity: 0.3;
                    fill-opacity: 0.05;
                    transition: fill 0.2s ease, border 0.1s ease;
                }

                .Cpoint circle {
                    stroke: #000000;
                    fill: #000000;
                    stroke-width: 0;
                }

                .point.gender-male circle {
                    stroke: #55bfff;
                    fill: #55bfff;
                }

                .point.gender-female circle {
                    stroke: #ff455f;
                    fill: #ff455f;
                }

                .point text {
                    font-family: "Inter Tight", sans-serif;
                    fill: #000000;
                    stroke-width: 4;
                    text-shadow: 1px 1px 4px #fff, 1px -1px 4px #fff, -1px 1px 4px #fff, -1px -1px 4px #fff;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.s ease;
                }

                .point:hover circle {
                    stroke-opacity: 1;
                    fill-opacity: 1;
                }

                .point:hover text {
                    visibility: visible;
                    opacity: 1;
                }
                """
            ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAsis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPosition.x)
                , y 35
                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.xName ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30
                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.yName ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (drawChosenpoint xScaleLocal yScaleLocal model.xFunction model.yFunction model.chosendata model.xName model.yName
                ++ List.map (drawpoint xScaleLocal yScaleLocal model.xFunction model.xName model.yFunction model.yName) model.data
            )
        ]



stylesheet : Html.Html Msg
stylesheet =
  let
    styles = 
        """
        #scatterplot-nav {
            display: flex;
            flex-wrap: wrap;
            gap: 1em 3em;
            margin: -1.5em -1em 1em -1em;
            padding: 1em; 
            padding-top: 1.5em; 
            background: #f8f8f8;
            border-bottom: 1px solid #dddddd;
        }

        #scatterplot-nav > span {
            flex: 0 0 100%;
        }

        #scatterplot-nav > form {
            display: flex;
            flex: 1;
            gap: 1em;
        }

        #scatterplot-nav > form > label {
            flex: 0 33%;
        }

        #scatterplot-nav > form > select {
            flex: 1;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]

change : ((LungCancerPrediction -> Float, String) -> Msg) -> String -> Msg
change msg value =
    case value of
        "Nummer des Patienten" -> msg (.index, "Nummer des Patienten")
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
    [ Html.Attributes.id "scatterplot-nav" ]
    [ Html.span [] [ Html.text "Nutze die Dropdown-Menüs, um die x- und y-Achsen zu wechseln." ]
    , Html.form
        []
        [ Html.label [] [ Html.text "X-Achse:" ]
        , Html.select
            [ Html.Events.onInput (change ChangeX) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.xName == "Rauchen") ]
                    [ Html.text "Tabakrauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.xName == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.xName == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.xName == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.xName == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                 , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.xName == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.xName == "Stauballergie") ]
                    [ Html.text "Hausstaubmilbenallergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.xName == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
        ]
    ]
    , Html.form
        []
        [ Html.label [] [ Html.text "Y-Achse:" ]
        , Html.select
            [ Html.Events.onInput (change ChangeY) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Suchtmittel" ] 
                [ Html.option
                    [ Html.Attributes.value "Rauchen"
                    , Html.Attributes.selected (data.yName == "Rauchen") ]
                    [ Html.text "Tabakrauchen" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum"
                    , Html.Attributes.selected (data.yName == "Alkoholkonsum") ]
                    [ Html.text "Alkoholkonsum" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Sonstiges" ] 
                [ Html.option
                    [ Html.Attributes.value "genetisches Risiko"
                    , Html.Attributes.selected (data.yName == "genetisches Risiko") ]
                    [ Html.text "Genetisches Risiko" ]
                , Html.option
                    [ Html.Attributes.value "Adipositas"
                    , Html.Attributes.selected (data.yName == "Adipositas") ]
                    [ Html.text "Adipositas" ]
                , Html.option
                    [ Html.Attributes.value "Nummer des Patienten"
                    , Html.Attributes.selected (data.yName == "Nummer des Patienten") ]
                    [ Html.text "Patientennummer" ]
                 , Html.option
                    [ Html.Attributes.value "Alter des Patienten"
                    , Html.Attributes.selected (data.xName == "Alter des Patienten") ]
                    [ Html.text "Alter des Patienten" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Äußerliche Einflüsse" ] 
                [   Html.option
                    [ Html.Attributes.value "Stauballergie"
                    , Html.Attributes.selected (data.yName == "Stauballergie") ]
                    [ Html.text "Hausstaubmilbenallergie" ]
                , Html.option
                    [ Html.Attributes.value "Luftverschmutzung"
                    , Html.Attributes.selected (data.yName == "Luftverschmutzung") ]
                    [ Html.text "Luftverschmutzung" ]
                ]
            ]
        ]
    ]


view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Leider konnte der Scatterplot der Lungenkrebspatienten bezüglich ihrer Attribute nicht geladen werden."

        Loading ->
            Html.span
                []
                [ Html.text "Lade Scatterplot der Lungenkrebspatienten... "
                , FontAwesome.view (FontAwesome.styled [ FontAwesome.Attributes.spin ] FontAwesome.Solid.spinner)
                ]

        Success l ->
            --let
              --  lungCancerPrediction : XYData
              --  lungCancerPrediction =
              --      filterReducedLungCancerPrediction l.data .gender l.xFunction l.yFunction l.xName l.yName

            --in 
            Html.div
                []
                [ stylesheet
                , nav l
                , scatterplot l
                ]