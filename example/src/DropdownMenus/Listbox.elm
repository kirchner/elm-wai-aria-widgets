module DropdownMenus.Listbox
    exposing
        ( Model
        , Msg
        , init
        , subscriptions
        , update
        , view
        )

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

import Data exposing (locales)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Lazy as Html
import Views exposing (liChildren, viewCheckbox)
import Widget.Listbox as Listbox
import Widget.Listbox.Dropdown as Dropdown exposing (Dropdown)


type alias Model =
    { selectedLocale : Maybe String
    , dropdown : Dropdown
    , jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : Bool
    }


init : Model
init =
    { selectedLocale = Nothing
    , dropdown = Dropdown.closed
    , jumpAtEnds = True
    , closeAfterMouseSelection = True
    , separateFocus = True
    , selectionFollowsFocus = False
    , handleHomeAndEnd = True
    , typeAhead = True
    }


type Msg
    = NoOp
    | DropdownMsg (Dropdown.Msg String)
    | JumpAtEndsChecked Bool
    | CloseAfterMouseSelectionChecked Bool
    | SeparateFocusChecked Bool
    | SelectionFollowsFocusChecked Bool
    | HandleHomeAndEndChecked Bool
    | TypeAheadChecked Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DropdownMsg dropdownMsg ->
            let
                ( newDropdown, dropdownCmd, newSelection ) =
                    Dropdown.update
                        (updateConfig
                            model.jumpAtEnds
                            model.closeAfterMouseSelection
                            model.separateFocus
                            model.selectionFollowsFocus
                            model.handleHomeAndEnd
                            model.typeAhead
                        )
                        model.dropdown
                        locales
                        model.selectedLocale
                        dropdownMsg
            in
            ( { model
                | dropdown = newDropdown
                , selectedLocale = newSelection
              }
            , Cmd.map DropdownMsg dropdownCmd
            )

        JumpAtEndsChecked enabled ->
            ( { model | jumpAtEnds = enabled }
            , Cmd.none
            )

        CloseAfterMouseSelectionChecked enabled ->
            ( { model | closeAfterMouseSelection = enabled }
            , Cmd.none
            )

        SeparateFocusChecked enabled ->
            ( { model | separateFocus = enabled }
            , Cmd.none
            )

        SelectionFollowsFocusChecked enabled ->
            ( { model | selectionFollowsFocus = enabled }
            , Cmd.none
            )

        HandleHomeAndEndChecked enabled ->
            ( { model | handleHomeAndEnd = enabled }
            , Cmd.none
            )

        TypeAheadChecked enabled ->
            ( { model | typeAhead = enabled }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DropdownMsg (Dropdown.subscriptions model.dropdown)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.lazy2 viewDropdown model.dropdown model.selectedLocale
        , Html.lazy6 viewDropdownConfiguration
            model.jumpAtEnds
            model.closeAfterMouseSelection
            model.separateFocus
            model.selectionFollowsFocus
            model.handleHomeAndEnd
            model.typeAhead
        ]


viewDropdown : Dropdown -> Maybe String -> Html Msg
viewDropdown dropdown selection =
    Html.div
        [ Attributes.class "field" ]
        [ Html.label
            [ Attributes.id "locales-dropdown-label" ]
            [ Html.text "Locale" ]
        , Html.div
            [ Attributes.class "control" ]
            [ selection
                |> Dropdown.view viewConfig
                    { id = "locales-dropdown"
                    , labelledBy = "locales-dropdown-label"
                    }
                    dropdown
                    locales
                |> Html.map DropdownMsg
            ]
        , Html.p
            [ Attributes.class "help" ]
            [ Html.text <|
                case selection of
                    Nothing ->
                        "nothing selected"

                    Just selectedLocale ->
                        "currently selected: " ++ selectedLocale
            ]
        ]


viewDropdownConfiguration jumpAtEnds closeAfterMouseSelection separateFocus selectionFollowsFocus handleHomeAndEnd typeAhead =
    Html.div []
        [ Html.label
            [ Attributes.class "label" ]
            [ Html.text "Configuration" ]
        , viewCheckbox JumpAtEndsChecked
            jumpAtEnds
            "Jump at ends"
        , viewCheckbox CloseAfterMouseSelectionChecked
            closeAfterMouseSelection
            "Close after mouse selection"
        , viewCheckbox SeparateFocusChecked
            separateFocus
            "Separate focus for mouse and keyboard"
        , viewCheckbox SelectionFollowsFocusChecked
            selectionFollowsFocus
            "Selection follows focus"
        , viewCheckbox HandleHomeAndEndChecked
            handleHomeAndEnd
            "Handle Home and End keys"
        , viewCheckbox TypeAheadChecked
            typeAhead
            "Type ahead"
        ]



---- CONFIGURATION


updateConfig : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Dropdown.UpdateConfig String
updateConfig jumpAtEnds closeAfterMouseSelection separateFocus selectionFollowsFocus handleHomeAndEnd typeAhead =
    Dropdown.updateConfig identity
        { jumpAtEnds = jumpAtEnds
        , closeAfterMouseSelection = closeAfterMouseSelection
        , separateFocus = separateFocus
        , selectionFollowsFocus = selectionFollowsFocus
        , handleHomeAndEnd = handleHomeAndEnd
        , typeAhead =
            if typeAhead then
                Listbox.typeAhead 200 <|
                    \query value ->
                        String.toLower value
                            |> String.contains (String.toLower query)
            else
                Listbox.noTypeAhead
        }


viewConfig : Dropdown.ViewConfig String String
viewConfig =
    Dropdown.viewConfig identity
        { container = []
        , button =
            \{ maybeSelection, open } ->
                { attributes = [ Attributes.class "button" ]
                , children =
                    [ Html.span
                        [ Attributes.style "width" "100%"
                        , Attributes.style "text-align" "left"
                        ]
                        [ maybeSelection
                            |> Maybe.withDefault "Select a locale..."
                            |> Html.text
                        ]
                    ]
                }
        , ul = [ Attributes.class "dropdown-list" ]
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
        , liDivider =
            \text ->
                { attributes =
                    [ Attributes.class "divider" ]
                , children = [ Html.text text ]
                }
        }
