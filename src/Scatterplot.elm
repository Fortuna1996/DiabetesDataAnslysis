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

type Model
 = Error
 | Loading
 | Success
    { data : List Chocolate
    , xFunction : Chocolate -> Float
    , yFunction : Chocolate -> Float
    , xName : String
    , yName : String
    }

type alias Chocolate =
    { company : String
    , review_date : Float
    , salt : Float
    , rating : Float
    , beans : Float
    , cocoa_butter : Float
    , sugar : Float
    , sweetener_without_sugar : Float
    , lecithin : Float
    }
type Msg
    = GotText (Result Http.Error String)
    | ChangeX (Chocolate -> Float, String)
    | ChangeY (Chocolate -> Float, String)

type alias Point = 
    { pointName : String, x : Float, y : Float }
type alias XYData =
    { xDescription : String
    , yDescription : String
    , data : List Point
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
                    { url = "https://raw.githubusercontent.com/Fortuna1996/ChocolateBarVisualization/main/" ++ data
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

list : List String 
list = 
    [ "chocolate%20(bearbeitet).csv" ]

csvStringToData : String -> List Chocolate
csvStringToData csvR =
    Csv.parse csvR
        |> Csv.Decode.decodeCsv decodingChocolate
        |> Result.toMaybe
        |>Maybe.withDefault []

decodingChocolate : Csv.Decode.Decoder (Chocolate -> a) a
decodingChocolate =
        Csv.Decode.map Chocolate
            (Csv.Decode.field "company" Ok 
                
                |> Csv.Decode.andMap (Csv.Decode.field "review_date"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "salt"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "rating"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "beans"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "cocoa_butter"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "sugar"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "sweetener_without_suggar"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "lecithin"(String.toFloat >> Result.fromMaybe "error parsing string"))
            )
            -- hinzufügen update : Msg

        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = chocolateList [ fullText ], xFunction = .rating, yFunction = .salt, xName = "Bewertung", yName = "Kakaogehalt"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = x, yFunction = m.yFunction, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = m.xFunction, yFunction = y, xName = m.xName, yName = a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

chocolateList :List String -> List Chocolate
chocolateList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

filterReducedchocolate : List Chocolate -> (Chocolate -> String) -> (Chocolate -> Float) -> (Chocolate->Float) -> String -> String -> XYData 

filterReducedchocolate chocolateliste a b c x y =
    XYData x y (List.map (\n -> pointName n a b c x y) chocolateliste)
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
pointName : Chocolate -> (Chocolate -> String) -> (Chocolate -> Float) -> (Chocolate -> Float) -> String -> String -> Point
pointName chocolate u v x y z =
    Point (u chocolate ++ ", " ++ y ++ ": " ++ String.fromFloat (v chocolate) ++ ", " ++ z ++ ": " ++ String.fromFloat (x chocolate)) (v chocolate) (x chocolate)

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY yxPoint =
    g
        [
            class["point"]
            ,fontSize <| Px 15.0
            ,fontFamily ["Times New Roman"]
            ,transform
                [
                    Translate
                    (Scale.convert scaleX yxPoint.x)
                    (Scale.convert scaleY yxPoint.y)
                ]
        ]

        [
            circle [cx 0, cy 0, r 5] []
            , text_ [x 10, y -20, textAnchor AnchorMiddle] [Html.text yxPoint.pointName]
        ]

scatterplot : XYData -> Svg msg
scatterplot model =
    let
        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

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
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke-opacity: 0.2 ; stroke-width: 2 ; stroke: rgba(2, 19, 100, 0.8); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(154, 22, 90, 0.8); fill: rgb(65, 209, 204); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAsis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPosition.x)
                , y 35
                 , fontFamily [ "Times New Roman" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30
                , fontFamily [ "Times New Roman" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]
view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Unfortunately scatterplot Chocolatebar can not be open."

        Loading ->
            Html.text "Loading Chocolatebar"

        Success l ->
            let
                chocolate =
                    filterReducedchocolate l.data .company l.xFunction l.yFunction l.xName l.yName

            in 
            Html.div []
                [
                    ul[][
                        li[][
                            Html.text <| "Change the x axis, to explore different combinations"
                            , Html.button [onClick (ChangeX (.review_date, "Review-Datum"))] [Html.text "Review-Datum"]
                            , Html.button [onClick (ChangeX (.salt, "Kakaoanteil"))] [Html.text "Kakaoanteil"]
                            , Html.button [onClick (ChangeX (.rating, "Bewertung"))] [Html.text "Bewertung"]
                            , Html.button [onClick (ChangeX (.beans, "Kakaobohnen"))] [Html.text "Kakaobohnen"]
                            , Html.button [onClick (ChangeX (.cocoa_butter, "Kakaobutter"))] [Html.text "Kakaobutter"]
                            , Html.button [onClick (ChangeX (.sugar, "Zucker"))] [Html.text "Zucker"]
                            , Html.button [onClick (ChangeX (.sweetener_without_sugar, "Süßungsmittel"))] [Html.text "Süßungsmittel"]
                            , Html.button [onClick (ChangeX (.lecithin, "Lecithin"))] [Html.text "Lecithin"]
                       ]
                    ]
                    , ul[][
                        li[][
                            Html.text <| "Change the y axis, to explore different combinations"
                            , Html.button [onClick (ChangeY (.review_date, "Review-Datum"))] [Html.text "Review-Datum"]
                            , Html.button [onClick (ChangeY (.salt, "Kakaoanteil"))] [Html.text "Kakaoanteil"]
                            , Html.button [onClick (ChangeY (.rating, "Bewertung"))] [Html.text "Bewertung"]
                            , Html.button [onClick (ChangeY (.beans, "Kakaobohnen"))] [Html.text "Kakaobohnen"]
                            , Html.button [onClick (ChangeY (.cocoa_butter, "Kakaobutter"))] [Html.text "Kakaobutter"]
                            , Html.button [onClick (ChangeY (.sugar, "Zucker"))] [Html.text "Zucker"]
                            , Html.button [onClick (ChangeY (.sweetener_without_sugar, "Süßungsmittel"))] [Html.text "Süßungsmittel"]
                            , Html.button [onClick (ChangeY (.lecithin, "Lecithin"))] [Html.text "Lecithin"]
                       ]
                    ]
                    ,   scatterplot chocolate
                ]
