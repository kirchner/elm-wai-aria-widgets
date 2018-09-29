module Main exposing (main)

{-

   Copyright 2018 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Html
import Widget.ComboBox as ComboBox exposing (ComboBox)
import Widget.Listbox as Listbox exposing (Entry)


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL


type alias Model =
    { comboBox : ComboBox
    , selection : Maybe String
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { comboBox = ComboBox.init
      , selection = Nothing
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = NoOp
    | ComboBoxMsg (ComboBox.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ComboBoxMsg comboBoxMsg ->
            let
                ( newComboBox, comboBoxCmd, newSelection ) =
                    ComboBox.update updateConfig
                        fruits
                        comboBoxMsg
                        model.comboBox
                        model.selection
            in
            ( { model
                | comboBox = newComboBox
                , selection = newSelection
              }
            , Cmd.map ComboBoxMsg comboBoxCmd
            )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ComboBoxMsg (ComboBox.subscriptions model.comboBox)



---- VIEW


view model =
    Html.div
        [ Attributes.class "section" ]
        [ Html.div
            [ Attributes.class "container" ]
            [ Html.div
                [ Attributes.class "field" ]
                [ Html.label
                    [ Attributes.id "fruits-label" ]
                    [ Html.text "Fruits" ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Html.map ComboBoxMsg <|
                        ComboBox.view viewConfig
                            { id = "fruits"
                            , labelledBy = "fruits-label"
                            }
                            fruits
                            model.comboBox
                            model.selection
                    ]
                , Html.p
                    [ Attributes.class "help" ]
                    [ Html.text <|
                        case model.selection of
                            Nothing ->
                                "nothing selected"

                            Just selection ->
                                "currently selected: " ++ selection
                    ]
                ]
            ]
        ]



---- CONFIG


sharedConfig : ComboBox.Shared String
sharedConfig =
    { uniqueId = identity
    , matchesQuery =
        \query option ->
            String.toLower option
                |> String.contains (String.toLower query)
    , printEntry = identity
    }


updateConfig : ComboBox.UpdateConfig String
updateConfig =
    ComboBox.updateConfig sharedConfig
        { jumpAtEnds = True
        , closeAfterMouseSelection = False
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , displayCondition = ComboBox.onFocus
        , minimalGap = 30
        , initialGap = 100
        }


viewConfig : ComboBox.ViewConfig String Never
viewConfig =
    ComboBox.viewConfig sharedConfig
        { container = []
        , placeholder = "Select a fruit.."
        , textfield = \_ -> [ Attributes.class "textfield" ]
        , ul = [ Attributes.class "dropdown-list" ]
        , liOption =
            \{ selected, focused, hovered, maybeQuery } name ->
                { attributes =
                    [ Attributes.class "entry"
                    , Attributes.classList
                        [ ( "entry--selected", selected )
                        , ( "entry--keyboard-focused", focused )
                        , ( "entry--mouse-focused", hovered )
                        ]
                    ]
                , children = liChildren maybeQuery name
                }
        , liDivider = Listbox.noDivider
        }


liChildren : Maybe String -> String -> List (Html Never)
liChildren maybeQuery name =
    case maybeQuery of
        Nothing ->
            [ Html.text name ]

        Just query ->
            let
                queryLength =
                    String.length query
            in
            String.toLower name
                |> String.split (String.toLower query)
                |> List.map String.length
                |> List.foldl
                    (\count ( remainingName, nodes ) ->
                        case remainingName of
                            "" ->
                                ( remainingName, nodes )

                            _ ->
                                ( String.dropLeft (count + queryLength) remainingName
                                , Html.span
                                    [ Attributes.style "color" "#0091eb" ]
                                    [ Html.text (String.left queryLength (String.dropLeft count remainingName)) ]
                                    :: Html.text (String.left count remainingName)
                                    :: nodes
                                )
                    )
                    ( name, [] )
                |> Tuple.second
                |> List.reverse



---- DATA


fruits : List (Entry String divider)
fruits =
    List.map Listbox.option
        [ "Açaí"
        , "Apple"
        , "Akee"
        , "Apricot"
        , "Avocado"
        , "Banana"
        , "Bilberry"
        , "Blackberry"
        , "Blackcurrant"
        , "Black sapote"
        , "Blueberry"
        , "Boysenberry"
        , "Buddha's hand (fingered citron)"
        , "Crab apples"
        , "Currant"
        , "Cherry"
        , "Cherimoya (Custard Apple)"
        , "Chico fruit"
        , "Cloudberry"
        , "Coconut"
        , "Cranberry"
        , "Cucumber"
        , "Damson"
        , "Date"
        , "Dragonfruit (or Pitaya)"
        , "Durian"
        , "Elderberry"
        , "Feijoa"
        , "Fig"
        , "Goji berry"
        , "Gooseberry"
        , "Grape"
        , "Grapefruit"
        , "Guava"
        , "Honeyberry"
        , "Huckleberry"
        , "Jabuticaba"
        , "Jackfruit"
        , "Jambul"
        , "Japanese plum"
        , "Jostaberry"
        , "Jujube"
        , "Juniper berry"
        , "Kiwano (horned melon)"
        , "Kiwifruit"
        , "Kumquat"
        , "Lemon"
        , "Lime"
        , "Loquat"
        , "Longan"
        , "Lychee"
        , "Mango"
        , "Mangosteen"
        , "Marionberry"
        , "Melon"
        , "Miracle fruit"
        , "Mulberry"
        , "Nectarine"
        , "Nance"
        , "Olive"
        , "Orange"
        , "Papaya"
        , "Passionfruit"
        , "Peach"
        , "Pear"
        , "Persimmon"
        , "Plantain"
        , "Plum"
        , "Pineapple"
        , "Pineberry"
        , "Plumcot (or Pluot)"
        , "Pomegranate"
        , "Pomelo"
        , "Purple mangosteen"
        , "Quince"
        , "Raspberry"
        , "Rambutan (or Mamin Chino)"
        , "Redcurrant"
        , "Salal berry"
        , "Salak"
        , "Satsuma"
        , "Soursop"
        , "Star apple"
        , "Star fruit"
        , "Strawberry"
        , "Surinam cherry"
        , "Tamarillo"
        , "Tamarind"
        , "Ugli fruit"
        , "Yuzu"
        ]
