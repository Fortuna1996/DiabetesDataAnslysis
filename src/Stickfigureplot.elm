module Stickfigureplot exposing (..)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (g, polyline, style, svg, text_)
import TypedSvg.Attributes as TSA exposing (class, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types as TST exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px) 
import Browser
import Html exposing (div, h1, p, button)
import Html.Events exposing (onClick)
import Http
import Csv
import Csv.Decode
import Statistics
import Html.Attributes as HA
import Html.Attributes
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Attributes

import Patients exposing (..)

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

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


stylesheet : Html.Html Msg
stylesheet =
  let
    styles = 
        """
        #stickfigure-nav {
            display: flex;
            flex-wrap: wrap;
            align-items: baseline;
            gap: 1em 3em;
            margin: -1.5em -1em 1em -1em;
            padding: 1em; 
            padding-top: 1.5em; 
            background: #f8f8f8;
            border-bottom: 1px solid #dddddd;
        }

        #stickfigure-nav > span {
            flex: 0 0 100%;
        }

        #stickfigure-nav > form {
            display: flex;
            flex: 1;
            gap: 1em;
        }

        #stickfigure-nav > form > label {
            flex: 0 33%;
        }

        #stickfigure-nav > form > select {
            flex: 1;
        }

        #stickfigure-nav > form > .size-selector {
            position: relative;
            flex: 1;
        }

        #stickfigure-nav > form > .size-selector > input {
            width: 100%;
        }

        #stickfigure-nav > form > .size-selector > span {
            position: absolute;
            top: -1.7em;
            left: 50%;
            transform: translateX(-50%);
            background: #fff;
            border-radius: 0.3em;
            padding: 0.3em;
            visibility: hidden;
            opacity: 0;
            transition: opacity 0.2s ease;
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.4);
        }

        #stickfigure-nav > form > .size-selector:hover > span {
            visibility: visible;
            opacity: 1;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]


nav : Data -> Html Msg
nav data = Html.nav
    [ Html.Attributes.id "stickfigure-nav" ]
    [ Html.span [] [ Html.text "Die Risiken für die Lungenkrebspatienten werden kategorisieren. Klicke das Dropdown-Menü an, um die Risiken auszuwählen. Es werden Attribute in Relation gesetzt. Diese sind an den x- & y-Achsen abzulesen. Mithilfe des Reglers kann die Größe der Stickfiguren je nach Bedarf angepasst werden." ]
    , Html.form
        []
        [ Html.label [] [ Html.text "Risiken:" ]
        , Html.select
            [ Html.Events.onInput ChangeRisk ]
            [ Html.option
                [ Html.Attributes.value "Körper"
                , Html.Attributes.selected (data.gr == "Körper") ]
                [ Html.text "Körper" ]
            , Html.option
                [ Html.Attributes.value "Rauchen"
                , Html.Attributes.selected (data.gr == "Rauchen") ]
                [ Html.text "Rauchen" ]
            , Html.option
                [ Html.Attributes.value "Erkrankung"
                , Html.Attributes.selected (data.gr == "Erkrankung") ]
                [ Html.text "Erkrankung" ]
            ]
        ]
    , Html.form
        []
        [ Html.label [] [ Html.text "Stickfigure-Größe:" ]
        , Html.div 
            [ Html.Attributes.class "size-selector" ]
            [ Html.input 
                [ HA.type_ "range"
                , HA.min "5"
                , HA.max "15"
                , HA.value <| String.fromFloat data.len
                , Html.Events.onInput ChangeLen
                ]
                []
            , Html.span
                [ Html.Attributes.style "left" (String.fromFloat((data.len - 2) / (15.5 - 1.5) * 100 + 4.5) ++ "%") ]
                [ text <| String.fromFloat data.len ]
            ]
        ]
    ]

view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Leider konnte der Stickfigureplot der Lungenkrebspatienten bezüglich ihrer Attribute nicht geladen werden."

        Loading ->
            Html.span
                []
                [ Html.text "Lade Stickfigureplot der Lungenkrebspatienten... "
                , FontAwesome.view (FontAwesome.styled [ FontAwesome.Attributes.spin ] FontAwesome.Solid.spinner)
                ]

        Success l ->
            let
                filteredLung =
                     l.data 

                numberStudies =
                    List.length l.data

            in
                div []
                    [ stylesheet
                    , nav l   
                    , p
                        []
                        [ text "Anzahl der Lungenkrebspatienten: "
                        , text <| String.fromInt numberStudies
                        ]
                    , stickfigureplot filteredLung l.chosendata l.len l.gr
                ]
gr : String
gr = "Erkrankung"



type alias Data =
    { data : List LungCancerPrediction
    , len : Float
    , gr : String
    , chosendata : Maybe LungCancerPrediction
    }

type Model
 = Error
 | Loading
 | Success Data


inDegree : List Float -> List Float
inDegree listvalue =
    List.map (\x -> (180 * (x - (Maybe.withDefault 0 (List.minimum listvalue)))/(((Maybe.withDefault 10000 (List.maximum listvalue))) - ((Maybe.withDefault 0 (List.minimum listvalue)))))) listvalue 

inDegree2 : Float -> (Float,Float)-> Float
inDegree2 x (min,max) =
    (180 * (x - ((min)))/((((max))) - (((min)))))


type alias LungCancerPrediction =
    { gender : Gender
    , index : Float
    , age : Float
    , airPollution : Float
    , alcoholUse : Float
    , dustAllergy : Float
    , geneticRisk : Float
    , obesity : Float
    , smoking : Float
    , passiveSmoker : Float
    , chronicLungDisease : Float
    , balancedDiet : Float
    , chestPain : Float
    }
type Msg
    = GotText (Result Http.Error String)
    | ChangeLen (String)
    | ChangeRisk (String)
    | PointChosen (Maybe LungCancerPrediction)


type alias Point =
    { pointName : String, x : Float, y : Float, z : Float, a : Float, b : Float , c : Float , d : Float , e : Float , f : Float , g : Float , h : Float , i : Float, gender: Gender } 

type alias XYData =
    { data : List Point
    , chosendata : Maybe Point
    }
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
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = lungCancerPredictionList [ fullText ], len=8, gr="Körper", chosendata = Nothing}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeLen v ->
            case model of
                Success m ->
                    (Success <| {data = m.data, len = Maybe.withDefault 0 <| String.toFloat v, gr=m.gr, chosendata = m.chosendata}, Cmd.none)
                _ ->
                    ( model, Cmd.none )
        ChangeRisk g ->
            case model of
                Success m ->
                    (Success <| {data = m.data, len = m.len, gr=g, chosendata = m.chosendata}, Cmd.none)
                _ ->
                    ( model, Cmd.none )

        PointChosen sac ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, chosendata = sac ,len = m.len, gr=m.gr}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
                   
lungCancerPredictionList :List String -> List LungCancerPrediction
lungCancerPredictionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

{- filterReducedLungCancerPrediction : List LungCancerPrediction -> XYData 

filterReducedLungCancerPrediction my_lung =
    XYData <| List.filterMap lung2point my_lung
 -}
andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap = Maybe.map2 (|>)

lung2point : LungCancerPrediction -> Maybe Point
lung2point lung =
    Maybe.map pointLabel 
        (Just lung.gender) 
            |> andMap (Just lung.index)
            |> andMap (Just lung.age) 
            |> andMap (Just lung.airPollution)
            |> andMap (Just lung.alcoholUse)
            |> andMap (Just lung.dustAllergy)
            |> andMap (Just lung.geneticRisk)
            |> andMap (Just lung.obesity)
            |> andMap (Just lung.smoking)
            |> andMap (Just lung.passiveSmoker)
            |> andMap (Just lung.chronicLungDisease)
            |> andMap (Just lung.balancedDiet)
            |> andMap (Just lung.chestPain)


genderLabel : Gender -> String
genderLabel gender = case gender of 
    M -> "männlich"
    F -> "weiblich"
    _ -> "unbekannt"

genderFlag :  String -> Gender
genderFlag gender = case gender of 
    "M" -> M
    "F" -> F
    _ -> UnknownGender

pointLabel : Gender -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Point 
pointLabel gender obesity smoking passiveSmoker chronicLungDisease balancedDiet chestPain index age airPollution geneticRisk alcoholUse dustAllergy= 
    Point ((genderLabel gender) ++ ", " ++ "Patientennummer: " ++  String.fromFloat index  ++ ", " ++ "Patientenalter: " ++ String.fromFloat age ++ "Jahre alt, " ++ "Luftverschmutzung: " ++ String.fromFloat airPollution ++ ", " 
        ++ "Alkoholkonsum: " ++ String.fromFloat alcoholUse ++ ", " ++ "Stauballergie: " ++ String.fromFloat dustAllergy ++ "") 
        (smoking) (obesity) (passiveSmoker) (chronicLungDisease) (balancedDiet) (chestPain) (index) (age) (geneticRisk) (airPollution) (dustAllergy) (alcoholUse) ( gender)
xAxis : List Float -> Svg msg
xAxis values = 
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
stickfigureplot : List LungCancerPrediction -> Maybe LungCancerPrediction->  Float -> String -> Svg Msg
stickfigureplot listlung mchosen len risk =

 -- funktionen und parameter deklarieren
    let

        xfunc =
            case risk of
                "Körper" -> .obesity 
                "Rauchen" -> .smoking
                _ -> .chronicLungDisease 
        yfunc =
            case risk of
                "Körper" -> .balancedDiet 
                "Rauchen" ->  .passiveSmoker 
                _ -> .chestPain 
        
        xValues : List Float
        xValues =
            List.map xfunc listlung

        yValues : List Float
        yValues =
            List.map yfunc listlung

        uValues : List Float
        uValues =
            List.map .airPollution listlung --u

        vValues : List Float
        vValues =
            List.map .alcoholUse listlung --v

        pValues : List Float
        pValues =
            List.map .dustAllergy listlung --p

        qValues : List Float
        qValues =
            List.map .index listlung --q

        zValues : List Float
        zValues =
            List.map .age listlung --z

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        uDegree : List Float
        uDegree = 
            List.map (\x -> (270 - (x))) (inDegree uValues)

        vDegree : List Float
        vDegree = 
            List.map (\x -> (270 - (x))) (inDegree vValues)

        pDegree : List Float
        pDegree = 
            List.map (\x -> (270 - (x))) (inDegree pValues)

        qDegree : List Float
        qDegree = 
            List.map (\x -> (270 - (x))) (inDegree qValues)

        zDegree : List Float
        zDegree = 
            List.map (\x -> (270 - (x))) (inDegree zValues)

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) *8/9

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = (wideExtent xValues |> half) 
            , y = (wideExtent yValues |> Tuple.second)
            }
    in
    -- output als svg => scatter plot
    
    svg [ viewBox 0 0 w h, TSA.width <| TST.Percent 100, TSA.height <| TST.Percent 100 ]
        [ style
            []
            [ TypedSvg.Core.text
                """
                .line polyline {
                    stroke: #dddddd;
                    stroke-width: 1;
                    transition: stroke 0.15s ease;
                }

                .Cline polyline {
                    stroke: #000000; 
                    stroke-width: 4; ----farbe ändern für angeklicte sticks
                }

                .line.gender-male polyline {
                    stroke: #55bfff;
                }

                .line.gender-female polyline {
                    stroke: #ff455f;
                }

                .line text {
                    font-size: small;
                    font-family: "Inter Tight", sans-serif;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.2s ease;
                }

                .line:hover polyline {
                    stroke: #333333;
                    stroke-width: 2;
                }

                .line:hover text {
                    visibility: visible;
                    opacity: 1;
                }
                """
            ]       
    -- plot x axis    
         , g[ transform [ Translate (60) (390)]]
            [
                xAxis xValues
                , text_
                [ x ( Scale.convert xScaleLocal (labelPositions.x))
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text (if risk == "Körper" then "Adipositas"
                else if risk == "Rauchen" then "Tabakrauchen"
                else "Brustschmerzen")] -- x -- xmts
                ]
    -- plot y axis             
         ,g[transform [Translate(60) (60)]]
         [
             yAxis yValues
             , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text (if risk == "Körper" then "Ausgewogene Ernährung"
                else if risk == "Rauchen" then "Passives Rauchen"
                else "Chronisch obstruktive Lungenerkrankung") ] -- y -- xmts
             ]
    -- plot points and description     
         ,g [ transform [ Translate padding padding ] ]
            (List.map (stickfigure xScaleLocal yScaleLocal len xfunc yfunc 
                (Maybe.withDefault 0 (List.minimum uValues), Maybe.withDefault 1000 (List.maximum uValues)) 
                (Maybe.withDefault 0 (List.minimum vValues), Maybe.withDefault 1000 (List.maximum vValues)) 
                (Maybe.withDefault 0 (List.minimum pValues), Maybe.withDefault 1000 (List.maximum pValues)) 
                (Maybe.withDefault 0 (List.minimum qValues), Maybe.withDefault 1000 (List.maximum qValues)) 
                (Maybe.withDefault 0 (List.minimum zValues), Maybe.withDefault 1000 (List.maximum zValues)))
            listlung
                {- xValues 
                |> andMapl yValues
                |> andMapl uDegree 
                |> andMapl vDegree 
                |> andMapl pDegree 
                |> andMapl qDegree 
                |> andMapl zDegree 
                |> andMapl model.data -}
                
            )
            -- map data with the defined variables
        ,g [ transform [ Translate padding padding ] ]
            (drawChosenStickfigure xScaleLocal yScaleLocal len xfunc yfunc 
                (Maybe.withDefault 0 (List.minimum uValues), Maybe.withDefault 1000 (List.maximum uValues)) 
                (Maybe.withDefault 0 (List.minimum vValues), Maybe.withDefault 1000 (List.maximum vValues)) 
                (Maybe.withDefault 0 (List.minimum pValues), Maybe.withDefault 1000 (List.maximum pValues)) 
                (Maybe.withDefault 0 (List.minimum qValues), Maybe.withDefault 1000 (List.maximum qValues)) 
                (Maybe.withDefault 0 (List.minimum zValues), Maybe.withDefault 1000 (List.maximum zValues))
            mchosen)
        
        ]

andMapl : List a -> List (a -> b) -> List b
andMapl = List.map2 (|>)

stickfigure : ContinuousScale Float -> ContinuousScale Float -> Float -> (LungCancerPrediction->Float)->(LungCancerPrediction->Float)-> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> LungCancerPrediction -> Svg Msg
stickfigure scaleX scaleY lange xfunc yfunc   (umin,umax) (vmin,vmax) (pmin,pmax) (qmin,qmax) (zmin,zmax) sacC =
    let
        degf : Float -> (Float,Float) -> Float
        degf = (\x (min,max) -> (270 - (inDegree2 x (min,max))))

        uDegree = degf sacC.airPollution (umin,umax)
        
        vDegree = degf sacC.alcoholUse (vmin,vmax)

        pDegree =  degf sacC.dustAllergy (pmin,pmax)

        qDegree = degf sacC.index (qmin,qmax)

        zDegree = degf sacC.age (zmin,zmax)

    in
        g [ class 
            [ "line"
            , case sacC.gender of
                M -> "gender-male"
                F -> "gender-female"
                UnknownGender -> "gender-unknown"
            ] ]
          [
            g  
                [ transform [ Translate (padding) padding ]
                ]
                [ text_ [ x  350, y -100, textAnchor AnchorMiddle ]
                    
                    [ Html.text ((genderLabel sacC.gender) ++ ", " ++ "Patientennummer: " ++ String.fromFloat sacC.index  ++ ", " ++ "Alter " ++ String.fromFloat sacC.age ++ "Jahre alt, " ++ "Luftverschmutzung: " ++ String.fromFloat sacC.airPollution ++ ", " ++ "Alkoholkonsum: " ++ String.fromFloat sacC.alcoholUse++ ", " ++ "Hausstaubmilbenallergie: " ++ String.fromFloat sacC.dustAllergy ++ "" ) ]
                ]
            , g
                [   transform
                    [ Translate
                        (Scale.convert scaleX (xfunc sacC)) 
                        (Scale.convert scaleY (yfunc sacC))  
                    ]
                    ,Html.Events.onClick (PointChosen (Just sacC))
                ]
                
                [ polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                        ]
                        []
                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                        ]
                        []
                    
                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                        ]
                        []
                ]
          ]


drawChosenStickfigure : ContinuousScale Float -> ContinuousScale Float -> Float -> (LungCancerPrediction->Float)->(LungCancerPrediction->Float)-> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Maybe LungCancerPrediction -> List (Svg Msg)
drawChosenStickfigure scaleX scaleY lange xfunc yfunc   (umin,umax) (vmin,vmax) (pmin,pmax) (qmin,qmax) (zmin,zmax) msacC =
    
        case msacC of 
        Nothing -> []
        Just sacC ->
            let
                degf : Float -> (Float,Float) -> Float
                degf = (\x (min,max) -> (270 - (inDegree2 x (min,max))))

                uDegree = degf sacC.airPollution (umin,umax)
                
                vDegree = degf sacC.alcoholUse (vmin,vmax)

                pDegree =  degf sacC.dustAllergy (pmin,pmax)

                qDegree = degf sacC.index (qmin,qmax)

                zDegree = degf sacC.age (zmin,zmax)

            in        
                [g [ class 
                    [ "Cline"
                    , case sacC.gender of
                        M -> "gender-male"
                        F -> "gender-female"
                        UnknownGender -> "gender-unknown"
                    ] ]
                    [
                    g
                        [   transform
                            [ Translate
                                (Scale.convert scaleX (xfunc sacC)) 
                                (Scale.convert scaleY (yfunc sacC)) 
                                 
                            ]
                            ,Html.Events.onClick (PointChosen (Nothing))
                        ]
                        
                        [ polyline
                                [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                                ]
                                []
                        , polyline
                                [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                                ]
                                []

                        , polyline
                                [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                                ]
                                []

                        , polyline
                                [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                                ]
                                []
                            
                        , polyline
                                [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                                ]
                                []
                        ]
                    ]
                ]

{- drawChosenpoint : ContinuousScale Float -> ContinuousScale Float -> (LungCancerPrediction -> Float) -> (LungCancerPrediction -> Float) -> Maybe LungCancerPrediction -> List (Svg Msg)
drawChosenpoint scaleX scaleY xfunc yfunc msac =
    case msac of
        Nothing -> []
        Just sac -> 
            [g
                [
                    class
                        [ "cpoint"
                        , case sac.gender of
                            M -> "gender-male"
                            F -> "gender-female"
                            UnknownGender -> "gender-unknown"
                        ]
                    ,fontSize <| Px 15.0
                    ,transform
                        [
                            Translate
                            (Scale.convert scaleX (xfunc sac))
                            (Scale.convert scaleY (yfunc sac))
                        ]
                ]

                [ polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                        ]
                        []
                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                        ]
                        []
                    
                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                        ]
                        []
                ]
            ] -}
