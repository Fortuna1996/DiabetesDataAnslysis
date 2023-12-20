module Main exposing (..)
import Browser
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Styles
import Html
import Html.Events exposing (onClick)
import Html.Attributes
import Stickfigureplot
import ParallelCoordinates
import Scatterplot
import Scatterplot exposing (scatterplot)
import FontAwesome.Brands



type alias Model =
    { 
      scatterplotModel : Scatterplot.Model
    , parallelCoordinatesModel : ParallelCoordinates.Model
    , stickfigureplotModel : Stickfigureplot.Model
    , active : Active
    }


type Active
    = Text
    | Scatterplot
    | ParallelCoordinates
    | Stickfigureplot


type Msg
     = ScatterplotMsg Scatterplot.Msg
    | ParallelCoordinatesMsg ParallelCoordinates.Msg
    | StickfigureplotMsg Stickfigureplot.Msg
    | SwitchView Active


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
    let
        (scatterplot, scatterplotCmd) = Scatterplot.init ()
        (parallelCoordinates, parallelCoordinatesCmd) = ParallelCoordinates.init ()
        (stickfigureplot, stickfigureplotCmd) = Stickfigureplot.init ()
    in
    ( Model scatterplot parallelCoordinates stickfigureplot Stickfigureplot 
    , Cmd.batch [ Cmd.map ScatterplotMsg scatterplotCmd, Cmd.map ParallelCoordinatesMsg parallelCoordinatesCmd, Cmd.map StickfigureplotMsg stickfigureplotCmd ])

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

--

style : Html.Html Msg
style =
  let
    styles = 
        """
        @import url("https://fonts.googleapis.com/css2?family=Inter+Tight&family=Inter:wght@400;600&display=swap");
        body {
            font-family: "Inter", sans-serif;
            width: 100vw;
            height: 100vh;
            background: #f0f0f0;
        }

        * {
            font-family: inherit;
        }

        #wrapper {
            display: flex;
            flex-direction: column;
            width: 100%;
            max-width: 1000px;
            height: 100vh;
            margin: 0 auto;
            background: #ffffff;
            box-shadow: 0 0 16px 0 rgba(0,0,0,0.4);
        }

        #nav {
            display: flex;
            z-index: 10;
            flex: 0;
            background: #a255ff;
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.4);
        }

        #nav-logo {
            line-height: 1em;
            padding: 0.5em;
            font-weight: 600;
            font-size: 1.5em;
        }

        #nav-title {
            flex: 1;
            line-height: 1em;
            padding: 1em;
            font-weight: 600;
            font-size: 1em;
            font-variant: small-caps;
        }

        #nav > button, #nav > a {
            display: flex;
            align-items: center;
            box-sizing: border-box;
            height: 100%;
            padding: 0 1em;
            background: transparent;
            border: none;
            border-top: 2px solid transparent;
            border-bottom: 2px solid transparent;
            font-size: 0.9em;
            transition: background 0.2s ease, border 0.2s ease;
        }

        #nav > button.active, #nav > a.active {
            background: #ffffff44;
            border-bottom-color: #000000;
        }

        #nav > button:hover, #nav > a:hover {
            background: #ffffffaa;
            border-bottom-color: #666666;
        }

        #nav > a:visited {
            color: unset;
        }

        #main {
            flex: 1;
            padding: 1em;
            padding-top: 1.5em;
            overflow: scroll;
        }

        #start-screen-nav {
            display: flex;
        }

        #start-screen-nav > button {
            height: 2.5em;
            border-radius: 1.25em;
            margin-right: 1em;
            padding: 0 1em;
            background: #a255ff3b;
            border: 2px solid #a255ff;
            font-size: 0.9em;
            transition: background 0.2s ease;
        }

        #start-screen-nav > button:hover {
            background: #a255ff;
        }

        #footer {
            display: flex;
            z-index: 10;
            flex: 0;
            font-size: 0.75em;
            padding: 1em;
            background: #f4f4f4;
            box-shadow: 0 -4px 8px 0 rgba(0,0,0,0.2);
            color: #444444;
        }

        #footer a, #footer a:visited {
            color: inherit;
        }

        #footer > p {
            flex: 1;
            margin: 0;
        }

        #footer > nav > a {
            margin-left: 1em;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]

nav : Model -> Html.Html Msg
nav model =
    Html.nav 
        [ Html.Attributes.id "nav" ] 
        [ Html.span 
            [ Html.Attributes.id "nav-logo"
            , onClick (SwitchView Text)
            ]
            [ FontAwesome.view FontAwesome.Solid.beer ]
        , Html.span 
            [ Html.Attributes.id "nav-title"
            , onClick (SwitchView Text)
            ] 
            [ Html.text " Risiken von Lungenkrebspatienten" ]
        , Html.button
            [ onClick (SwitchView Text)
            , Html.Attributes.class (if model.active == Text then "active" else "")
            ]
            [ FontAwesome.view FontAwesome.Solid.home ]
        , Html.button
            [ onClick (SwitchView Scatterplot)
            , Html.Attributes.class (if model.active == Scatterplot then "active" else "")
            ]
            [ Html.text "Scatterplot" ]
        , Html.button 
            [ onClick (SwitchView ParallelCoordinates)
            , Html.Attributes.class (if model.active == ParallelCoordinates then "active" else "")
            ]
            [ Html.text "Parallele Koordinaten" ]
        , Html.button 
            [ onClick (SwitchView Stickfigureplot)
            , Html.Attributes.class (if model.active == Stickfigureplot then "active" else "")
            ]
            [ Html.text "Stickfigures" ]
        , Html.a 
            [ Html.Attributes.href "https://github.com/Fortuna1996/LungCancerPrediction"
            , Html.Attributes.target "_blank"
            ]
            [ FontAwesome.view FontAwesome.Brands.github ] 
        ]

body : Model -> Html.Html Msg
body model =
    Html.main_ 
        [ Html.Attributes.id "main" ]
        [ case model.active of
            Text ->
                Html.div
                    [] 
                    [ Html.p
                        [] 
                        [ Html.text "Willkommen auf dieser Webseite!"
                        , Html.br [] []
                        , Html.br [] []
                        , Html.br [] []
                        , Html.text "Hier werden Risikofaktoren der Lungenkrebspatienten dargestellt und in unterschiedlichster Weise in Relation gesetzt. "
                        , Html.br [] []
                        , Html.text "Neugierig geworden? Finde mehr heraus! "
                        , Html.br [] []
                        , Html.br [] []
                        , Html.text "Hinweis: Bezüglich des Wortes Lungenkrebspatienten wird das generische Maskulinum verwendet und bezieht sich auf alle Geschlechter. Somit ist eine Benachteiligung jeglichen Geschlechts ausgeschlossen."
                        , Html.br [] []
                        , Html.br [] []
                        , Html.text "(Unten rechts stehen der Quellcode und die Daten. Beim Anklicken der jeweiligen Wörter werden genauere Informationen erhalten.) "
                        ]

                        
                        
                    , Html.p [] [ Html.text "Wähle folgende Visualisierungsdarstellung aus:" ]
                    , Html.nav
                        [ Html.Attributes.id "start-screen-nav" ]
                        [ Html.button
                            [ onClick (SwitchView Scatterplot) ]
                            [ Html.text "Scatterplot ",
                            FontAwesome.view FontAwesome.Solid.arrowRight
                            ]
                        , Html.button 
                            [ onClick (SwitchView ParallelCoordinates) ]
                            [ Html.text "Parallele Koordinaten ",
                            FontAwesome.view FontAwesome.Solid.arrowRight
                            ]
                        , Html.button 
                            [ onClick (SwitchView Stickfigureplot) ]
                            [ Html.text "Stickfigures ",
                            FontAwesome.view FontAwesome.Solid.arrowRight
                            ]
                        ]
                    ]

            Scatterplot ->
                Html.map ScatterplotMsg (Scatterplot.view model.scatterplotModel)

            ParallelCoordinates ->
                Html.map ParallelCoordinatesMsg (ParallelCoordinates.view model.parallelCoordinatesModel)

            Stickfigureplot ->
                Html.map StickfigureplotMsg (Stickfigureplot.view model.stickfigureplotModel)
        ]


footer : Model -> Html.Html Msg
footer model =
    Html.footer 
        [ Html.Attributes.id "footer" ]
        [ Html.p
            []
            [ FontAwesome.view FontAwesome.Solid.copyright
            , Html.text " 2023"
            ]
        , Html.nav
            []
            [ Html.a
                [ Html.Attributes.href "https://www.kaggle.com/datasets/thedevastator/cancer-patients-and-air-pollution-a-new-link"
                , Html.Attributes.target "_blank"
                ]
                [ Html.text "Daten" ]
            , Html.a
                [ Html.Attributes.href "https://github.com/Fortuna1996/LungCancerPrediction"
                , Html.Attributes.target "_blank"
                ]
                [ Html.text "Quellcode" ]
            ]
        ]


view : Model -> Html.Html Msg
view model =
    Html.div 
        []
        [ style
        , FontAwesome.Styles.css
        , Html.div 
            [ Html.Attributes.id "wrapper" ]
            [ nav model
            , body model
            , footer model
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of        
        ScatterplotMsg scatterplotMsg ->
            let
                (scatterplot, scatterplotCmd) = 
                    Scatterplot.update scatterplotMsg model.scatterplotModel
            in
            ( { model | scatterplotModel = scatterplot }, Cmd.none )

        ParallelCoordinatesMsg parallelCoordinatesMsg ->
            let
                (parallelCoordinates, parallelCoordinatesCmd) = ParallelCoordinates.update parallelCoordinatesMsg model.parallelCoordinatesModel
            in
            ( { model | parallelCoordinatesModel = parallelCoordinates }, Cmd.none )

        StickfigureplotMsg stickfigureplotMsg ->
            let
                (stickfigureplot, stickfigureplotCmd) = Stickfigureplot.update stickfigureplotMsg model.stickfigureplotModel
            in
            ( { model | stickfigureplotModel = stickfigureplot }, Cmd.none )

        SwitchView newActitve ->
            if model.active== Stickfigureplot then
                case (model.scatterplotModel, model.stickfigureplotModel) of 
                    (Scatterplot.Success spm, Stickfigureplot.Success sfm) ->
                        let 
                            chosen = sfm.chosendata 
                            newChosen = Maybe.map 
                                            (\x -> {    gender=x.gender,
                                                        index=x.index,
                                                        age=x.age,
                                                        airPollution=x.airPollution,
                                                        alcoholUse=x.alcoholUse,
                                                        dustAllergy=x.dustAllergy,
                                                        geneticRisk=x.geneticRisk,
                                                        obesity=x.obesity,
                                                        smoking=x.smoking,
                                                        passiveSmoker=x.passiveSmoker,
                                                        chronicLungDisease=x.chronicLungDisease,
                                                        balancedDiet=x.balancedDiet,
                                                        chestPain=x.chestPain
                                                    }) chosen
                            newmodel = {spm|chosendata=newChosen}

                        in
                        ( { model | scatterplotModel = Scatterplot.Success newmodel , active = newActitve}, Cmd.none )
                    _ -> 
                        ( { model | active = newActitve }, Cmd.none )
            else
                ( { model | active = newActitve }, Cmd.none )
