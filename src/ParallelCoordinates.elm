module ParallelCoordinates exposing (..)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
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



type Model
  = Fehlschlag
  | Laden
  | Erfolg 
    { data : List Weine
    , ersteFunktion : Weine -> Float
    , zweiteFunktion : Weine -> Float
    , dritteFunktion : Weine -> Float
    , vierteFunktion : Weine -> Float
    , ersterName : String
    , zweiterName : String
    , dritterName : String
    , vierterName : String
    }

type alias Weine =
    { name : String
    , alc : Float
    , temperatur : Float
    , suesse : Float
    , saeurengehalt : Float
    , koerper : Float
    , gerbstoff : Float
    , preis : Float
    , jahr : Float
    , ml : Float
    }

type Msg
    = ErhalteText (Result Http.Error String)
    | Ändere1 (Weine -> Float, String)
    | Ändere2 (Weine -> Float, String)
    | Ändere3 (Weine -> Float, String)
    | Ändere4 (Weine -> Float, String)

type alias MultiDimPunkt =
    { punktName : String, value : List Float }

type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPunkt)
    }



holenVonCsv : (Result Http.Error String -> Msg) -> Cmd Msg
holenVonCsv x = 
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/RicBre/Elm-Projekt-WineInformation/main/Daten/AufbereiteteDaten/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    [ "WineInformationExcelAufbereitetKlein.csv"]

csvStringZuDaten : String -> List Weine
csvStringZuDaten csvRoh =
    Csv.parse csvRoh
        |> Csv.Decode.decodeCsv dekodierenWeine
        |> Result.toMaybe
        |> Maybe.withDefault []

dekodierenWeine : Csv.Decode.Decoder (Weine -> a) a
dekodierenWeine =
    Csv.Decode.map Weine
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "alc"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "temperatur"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "suesse"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "saeurengehalt"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "koerper"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "gerbstoff"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "preis"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "jahr"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "ml"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )

weineListe :List String -> List Weine
weineListe liste1 =
    List.map(\t -> csvStringZuDaten t) liste1
        |> List.concat



abstand : Float
abstand =
    60

radius : Float
radius =
    5.0

einteilungAchseZahl : Int
einteilungAchseZahl =
    8

standartErweiterung : ( number, number1 )
standartErweiterung =
    ( 0, 100 )

weiteErweiterung : List Float -> ( Float, Float )
weiteErweiterung werte =
    let
        nahErweiterung =
            Statistics.extent werte
                |> Maybe.withDefault standartErweiterung

        erwiterung =
            (Tuple.second nahErweiterung - Tuple.first nahErweiterung) / toFloat (2 * einteilungAchseZahl)
    in
    ( Tuple.first nahErweiterung - erwiterung |> max 0
    , Tuple.second nahErweiterung + erwiterung
    )

paralleleKoordinatenPlan : Float -> Float -> MultiDimData -> Svg msg
paralleleKoordinatenPlan w ar model =
    let
        h : Float
        h =
            w / ar

        listeTransformieren : List (List Float)
        listeTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listeWeiteErweiterung : List ( Float, Float )
        listeWeiteErweiterung =
            listeTransformieren |> List.map weiteErweiterung

        listeSkala =
            List.map (Scale.linear ( h, 0 )) listeWeiteErweiterung

        listeAchse =
            List.map (Axis.left [ Axis.tickCount einteilungAchseZahl ]) listeSkala

        xSkala =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * abstand) (h + 2 * abstand)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style []
            [
                TypedSvg.Core.text """
                .parallelerPunkt { stroke: rgba(1, 0, 0,0.2);}
                .parallelerPunkt:hover {stroke: rgb(173, 255, 47); stroke-width: 2;} 
                .parallelerPunkt text { display: none; }
                .parallelerPunkt:hover text { display: inline; stroke: rgb(0, 0, 0); stroke-width: 0.1; font-size: small; font-family: calibri}  
                """
            ]
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (abstand - 1) abstand ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xSkala (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listeAchse
            , g [ transform [ Translate (abstand - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "calibri" ]
                            , fontSize (Px 12)
                            , x <| Scale.convert xSkala (toFloat i + 1)
                            , y <| abstand * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    zeichnePunkt p name beschreibung =
                        let
                            linienWeg : Path.Path
                            linienWeg =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xSkala <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listeSkala
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        g [class ["parallelerPunkt"]][
                            Path.element linienWeg
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            , class ["parallelerPunkt"]
                            ]
                            , text_
                                [ x 300
                                , y -20
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p beschreibung)))]
                                
                        ]
                        
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (abstand - 1) abstand ] ]
                                (List.map (\a -> zeichnePunkt a.value a.punktName model.dimDescription) dataset)
                        )
               )



main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Laden
    , holenVonCsv ErhalteText
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Fehlschlag ->
            Html.text "Ich konnte Ihre Weine nicht öffnen."

        Laden ->
            Html.text "Weine werden geöffnet..."

        Erfolg l ->
                    let
                        multiDimDaten : List Weine -> (Weine -> Float) -> (Weine -> Float) -> (Weine -> Float) -> (Weine -> Float) -> (Weine -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listeWeine a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPunkt (e x)
                                )
                                listeWeine
                            ]

                        plotDaten = 
                            multiDimDaten l.data l.ersteFunktion l.zweiteFunktion l.dritteFunktion l.vierteFunktion .name l.ersterName l.zweiterName l.dritterName l.vierterName       
                    in
                    Html.div []
                        [
                            ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die erste Spalte aus"
                                    , Html.button [onClick (Ändere1 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere1 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere1 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere1 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere1 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere1 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere1 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere1 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere1 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die zweite Spalte aus"
                                    , Html.button [onClick (Ändere2 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere2 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere2 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere2 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere2 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere2 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere2 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere2 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere2 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die dritte Spalte aus"
                                    , Html.button [onClick (Ändere3 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere3 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere3 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere3 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere3 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere3 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere3 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere3 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere3 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die vierte Spalte aus"
                                    , Html.button [onClick (Ändere4 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere4 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere4 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere4 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere4 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere4 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere4 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere4 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere4 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                             ]
                                ,paralleleKoordinatenPlan 600 2 plotDaten
                        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErhalteText ergebnis ->
            case ergebnis of
                Ok fullText ->
                    ( Erfolg <| { data = weineListe [ fullText ], ersteFunktion = .alc, zweiteFunktion = .temperatur, dritteFunktion = .suesse, vierteFunktion = .saeurengehalt , ersterName = "Alkohol", zweiterName = "Temperatur", dritterName = "Süße", vierterName = "Säuregehalt"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        Ändere1 (x, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = x, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = a, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere2 (y, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = y, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = a, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere3 (z, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = z, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = a, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere4 (c, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = c , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )