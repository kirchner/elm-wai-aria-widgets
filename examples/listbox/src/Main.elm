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
import Widget exposing (HtmlDetails)
import Widget.Listbox as Listbox exposing (Entry, Listbox)


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
    { listbox : Listbox
    , selection : Maybe String
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { listbox = Listbox.init
      , selection = Nothing
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = NoOp
    | ListboxMsg (Listbox.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    Listbox.updateUnique updateConfig
                        fruits
                        listboxMsg
                        model.listbox
                        model.selection
            in
            ( { model
                | listbox = newListbox
                , selection = newSelection
              }
            , Cmd.map ListboxMsg listboxCmd
            )



---- SUBSCRIPTIONS


subscriptions model =
    Sub.map ListboxMsg (Listbox.subscriptions model.listbox)



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
                    [ Listbox.viewUnique viewConfig
                        { id = "fruits"
                        , labelledBy = "fruits-label"
                        , lift = ListboxMsg
                        }
                        fruits
                        model.listbox
                        model.selection
                    ]
                ]
            ]
        ]



---- CONFIG


updateConfig : Listbox.UpdateConfig String
updateConfig =
    Listbox.updateConfig identity
        { jumpAtEnds = True
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = Listbox.simpleTypeAhead 200 identity
        , minimalGap = 30
        , initialGap = 100
        }


viewConfig : Listbox.ViewConfig String Never
viewConfig =
    Listbox.viewConfig identity
        { ul = [ Attributes.class "list" ]
        , liOption =
            \{ selected, keyboardFocused, mouseFocused, maybeQuery } name ->
                { attributes =
                    [ Attributes.class "entry"
                    , Attributes.classList
                        [ ( "entry--selected", selected )
                        , ( "entry--keyboard-focused", keyboardFocused )
                        , ( "entry--mouse-focused", mouseFocused )
                        ]
                    ]
                , children = liChildren maybeQuery name
                }
        , liDivider = Listbox.noDivider
        , empty = Html.div [] [ Html.text "this list is empty" ]
        , focusable = True
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
