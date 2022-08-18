module TreeHierarchy exposing (main)

import Color
import Dict
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Scale
import Statistics
import Tree exposing (Tree)
import TreeLayout
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Json.Decode
import Browser

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err error) ->
            ( { model
                | tree = Tree.singleton ""
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Some other Error"
              }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = Tree.singleton "", errorMsg = "Loading ..." }
    , Http.get { url = "data/countryHierarchy.json", expect = Http.expectJson GotFlare treeDecoder1 }
    )

view : Model -> Html msg
view model =
    let
        
        tree405 =
            Tree.tree "1"
                [ Tree.tree "2"
                    [ Tree.tree "3" 
                        [ Tree.tree "4" 
                            [Tree.tree "5" [] ]
                        ]
                    , Tree.tree "6" 
                        [Tree.tree "7" 
                            [Tree.tree "8" [] ]
                        , Tree.tree "9" []
                        ]
                    ]
                , Tree.tree "10" 
                    [Tree.tree "11" []
                    , Tree.tree "12" 
                        [Tree.tree "13"[] ]
                    ]
                ]
        convertedTestTree : List ( String, Maybe String )
        convertedTestTree =
            model.tree  
            -- tree405                
                |> Tree.map (\v -> ( v, Nothing ))
                |> convert
                |> Tree.flatten

        layoutTestTree : Dict.Dict String { x : Float, y : Float }
        layoutTestTree =

          TreeLayout.treeLayout 1 convertedTestTree
         
    in
    div []
        [ treePlot 1 convertedTestTree
        , Html.div [] [ Html.text "Converted Tree from flare.json" ]
        , Html.ul [] <|
            List.map
                (\( child, parent ) ->
                    Html.li []
                        [ Html.text <|
                            "(  "
                                ++ child
                                ++ ", "
                                ++ Maybe.withDefault "Nothing" parent
                                ++ ")"
                        ]
                )
                convertedTestTree
        , Html.div [] [ Html.text "Tree Layout" ]
        , Html.ul [] <|
            List.map
                (\( node, { x, y } ) ->
                    Html.li []
                        [ Html.text <|
                            "("
                                ++ node
                                ++ ", x="
                                ++ String.fromFloat x
                                ++ ", y="
                                ++ String.fromFloat y
                                ++ ")"
                        ]
                )
            <|
                Dict.toList layoutTestTree
        ]


convert : Tree ( String, Maybe String ) -> Tree ( String, Maybe String )
convert t =
    let
        ( currentLabel, _ ) =
            Tree.label t
    in
    Tree.mapChildren
        (\listChildren ->
            listChildren
                |> List.map
                    (\c ->
                        convert c
                            |> Tree.mapLabel (\( a, _ ) -> ( a, Just currentLabel ))
                    )
        )
        t

w : Float
w =
    1600


h : Float
h =
    900


padding : Float
padding =
    60


radius : Float
radius =
    5.0


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


xScale : List Float -> Scale.ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) <| (Statistics.extent values |> Maybe.withDefault defaultExtent)


yScale : List Float -> Scale.ContinuousScale Float
yScale values =
    Scale.linear ( 0, h - 2 * padding ) <| (Statistics.extent values |> Maybe.withDefault defaultExtent)


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * 10)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )

type alias LineData =
    { x : Float
    , y : Float
    , px : Float
    , py : Float
    , pointName :  String
    }


treePlot : Float -> List ( String, Maybe String ) -> Svg msg
treePlot minDist tree =
    let
        -- layout berechnen

        layout = TreeLayout.treeLayout minDist tree
        
        list : List LineData
        list = List.map (\(node,parent) ->
            let
                y = Dict.get node layout |> Maybe.map (\a -> a.y) |> Maybe.withDefault -1 
                x = Dict.get node layout |> Maybe.map (\a -> a.x) |> Maybe.withDefault -1 
                px = parent |> Maybe.andThen (\p -> Dict.get p layout) |> Maybe.map (\a -> a.x) |> Maybe.withDefault -1
                py = parent |> Maybe.andThen (\p -> Dict.get p layout) |> Maybe.map (\a -> a.y) |> Maybe.withDefault -1
                pointName = node
            in
                LineData x y px py pointName ) tree

        xValues : List Float
        xValues =
           List.map .x list

        yValues : List Float
        yValues =
            List.map .y list

        xScaleLocal : Scale.ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : Scale.ContinuousScale Float
        yScaleLocal =
            yScale yValues


        checkNegative data =
            if (data.px < 0) && (data.py < 0) then
                LineData 0 1 data.x data.y data.pointName

            else
                LineData data.x data.y data.px data.py data.pointName

        list1 : List LineData
        list1 =
            List.map checkNegative list

    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style []
            [ TypedSvg.Core.text """
            .point circle { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point line { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(118, 214, 78,1); }
            .point:hover text { display: inline; }
          """ ]
          
        , g
            [ transform [ Translate padding padding ] ]
            (List.map (drawLine xScaleLocal yScaleLocal) list1)
        , g
            [ transform [ Translate padding padding ] ]
            (List.map (drawPoint xScaleLocal yScaleLocal) list)
        ]

drawPoint : Scale.ContinuousScale Float -> Scale.ContinuousScale Float -> LineData -> Svg msg
drawPoint scaleX scaleY xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.x) -- x
                (Scale.convert scaleY xyPoint.y) -- y
            ]
            
        ]
        [ circle [ cx 0, cy 0, r 5 ] []
        , text_ [ x 0, y -10, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
        ]

drawLine : Scale.ContinuousScale Float -> Scale.ContinuousScale Float -> LineData -> Svg msg
drawLine scaleX scaleY xyPoint =
    g
        [ class [ "point" ] ]
        [ TypedSvg.line
            [ x1 (Scale.convert scaleX xyPoint.x)
            , y1 (Scale.convert scaleY xyPoint.y)
            , x2 (Scale.convert scaleX xyPoint.px)
            , y2 (Scale.convert scaleY xyPoint.py)
            ]
            []
        ]
treeDecoder1 : Json.Decode.Decoder (Tree String)
treeDecoder1 =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    Tree.tree name []

                Just c ->
                    Tree.tree name c
        )
        (Json.Decode.field "data" (Json.Decode.field "id" Json.Decode.string))
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder1)
        )

treeDecoder : Json.Decode.Decoder (Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    Tree.tree name []

                Just c ->
                    Tree.tree name c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )

type Msg
    = GotFlare (Result Http.Error (Tree String))

type alias Model =
    { tree : Tree String, errorMsg : String }



-- testTree = Tree.tree "root" [ Tree.tree "home" [Tree.tree "user1" [], Tree.tree "user2" []], Tree.tree "etc" [], Tree.tree "var" [Tree.tree "log" []]]
-- Tree "root" [Tree "home" [Tree "user1" [],Tree "user2" []],Tree "etc" [],Tree "var" [Tree "log" []]]
--     : Tree String
-- mappedTestTree = Tree.map (\v -> (v,Nothing)) testTree
-- Tree ("root",Nothing) [Tree ("home",Nothing) [Tree ("user1",Nothing) [],Tree ("user2",Nothing) []],Tree ("etc",Nothing) [],Tree ("var",Nothing) [Tree ("log",Nothing) []]]
--     : Tree ( String, Maybe a )
-- convert mappedTestTree
-- Tree ("root",Nothing) [Tree ("home",Just "root") [Tree ("user1",Just "home") [],Tree ("user2",Just "home") []],Tree ("etc",Just "root") [],Tree ("var",Just "root") [Tree ("log",Just "var") []]]
--     : Tree ( String, Maybe String )
-- cTree = convert mappedTestTree
-- Tree ("root",Nothing) [Tree ("home",Just "root") [Tree ("user1",Just "home") [],Tree ("user2",Just "home") []],Tree ("etc",Just "root") [],Tree ("var",Just "root") [Tree ("log",Just "var") []]]
--     : Tree ( String, Maybe String )
-- result = Tree.flatten cTree
-- [("root",Nothing),("home",Just "root"),("user1",Just "home"),("user2",Just "home"),("etc",Just "root"),("var",Just "root"),("log",Just "var")]
--     : List ( String, Maybe String )
-- import TreeLayout
-- TreeLayout.treeLayout 2 result
-- Dict.fromList [("etc",{ x = 0, y = 2 }),("home",{ x = -2, y = 2 }),("log",{ x = 2, y = 3 }),("root",{ x = 0, y = 1 }),("user1",{ x = -3, y = 3 }),("user2",{ x = -1, y = 3 }),("var",{ x = 2, y = 2 })]
--     : Dict.Dict String TreeLayout.Coordinate
