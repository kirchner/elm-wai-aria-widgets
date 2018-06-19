module DropdownMenus.ComboBox
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
import Helper exposing (matchesQuery)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Html
import Views exposing (liChildren, viewCheckbox)
import Widget.ComboBox as ComboBox exposing (ComboBox)


type alias Model =
    { selectedLocale2 : Maybe String
    , comboBox : ComboBox
    , jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , displayCondition : SelectedDisplayCondition
    }


type SelectedDisplayCondition
    = MatchingQuery Int
    | OnFocus
    | OnDemand


init : Model
init =
    { selectedLocale2 = Nothing
    , comboBox = ComboBox.closed
    , jumpAtEnds = True
    , closeAfterMouseSelection = True
    , separateFocus = True
    , selectionFollowsFocus = False
    , handleHomeAndEnd = True
    , displayCondition = MatchingQuery 3
    }


type Msg
    = ComboBoxMsg (ComboBox.Msg String)
    | JumpAtEndsChecked Bool
    | CloseAfterMouseSelectionChecked Bool
    | SeparateFocusChecked Bool
    | SelectionFollowsFocusChecked Bool
    | HandleHomeAndEndChecked Bool
    | DisplayConditionSelected SelectedDisplayCondition
    | MatchingQueryCountChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComboBoxMsg comboBoxMsg ->
            let
                ( newComboBox, comboBoxCmd, newSelection ) =
                    ComboBox.update
                        (updateConfig
                            model.jumpAtEnds
                            model.closeAfterMouseSelection
                            model.separateFocus
                            model.selectionFollowsFocus
                            model.handleHomeAndEnd
                            model.displayCondition
                        )
                        model.comboBox
                        locales
                        model.selectedLocale2
                        comboBoxMsg
            in
            ( { model
                | comboBox = newComboBox
                , selectedLocale2 = newSelection
              }
            , Cmd.map ComboBoxMsg comboBoxCmd
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

        DisplayConditionSelected selectedDisplayCondition ->
            ( { model | displayCondition = selectedDisplayCondition }
            , Cmd.none
            )

        MatchingQueryCountChanged rawCount ->
            case model.displayCondition of
                MatchingQuery _ ->
                    case String.toInt rawCount of
                        Nothing ->
                            ( model, Cmd.none )

                        Just count ->
                            ( { model | displayCondition = MatchingQuery count }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ComboBoxMsg (ComboBox.subscriptions model.comboBox)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.lazy2 viewComboBox model.comboBox model.selectedLocale2
        , Html.lazy6 viewComboBoxConfiguration
            model.jumpAtEnds
            model.closeAfterMouseSelection
            model.separateFocus
            model.selectionFollowsFocus
            model.handleHomeAndEnd
            model.displayCondition
        ]


viewComboBox : ComboBox -> Maybe String -> Html Msg
viewComboBox comboBox selection =
    Html.div
        [ Attributes.class "field" ]
        [ Html.label
            [ Attributes.id "locales-combo-box-label" ]
            [ Html.text "Locale" ]
        , Html.div
            [ Attributes.class "control" ]
            [ selection
                |> ComboBox.view viewConfig
                    { id = "locales-combo-box"
                    , labelledBy = "locales-combo-box-label"
                    }
                    comboBox
                    locales
                |> Html.map ComboBoxMsg
            ]
        , Html.p
            [ Attributes.class "help" ]
            [ Html.text <|
                case selection of
                    Nothing ->
                        "nothing selected"

                    Just actualSelection ->
                        "currently selected: " ++ actualSelection
            ]
        ]


viewComboBoxConfiguration jumpAtEnds closeAfterMouseSelection separateFocus selectionFollowsFocus handleHomeAndEnd displayCondition =
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
        , Html.div
            [ Attributes.class "field" ]
            [ Html.div
                [ Attributes.class "control" ]
                [ Html.label
                    [ Attributes.class "radio" ]
                    [ Html.input
                        [ Attributes.type_ "radio"
                        , Attributes.name "displayCondition"
                        , Attributes.checked <|
                            case displayCondition of
                                MatchingQuery _ ->
                                    True

                                _ ->
                                    False
                        , case displayCondition of
                            MatchingQuery count ->
                                Events.onClick
                                    (DisplayConditionSelected (MatchingQuery count))

                            _ ->
                                Events.onClick
                                    (DisplayConditionSelected (MatchingQuery 3))
                        ]
                        []
                    , Html.text " Matching query"
                    ]
                , Html.label
                    [ Attributes.class "radio" ]
                    [ Html.input
                        [ Attributes.type_ "radio"
                        , Attributes.name "displayCondition"
                        , Attributes.checked (displayCondition == OnFocus)
                        , Events.onClick (DisplayConditionSelected OnFocus)
                        ]
                        []
                    , Html.text " On focus"
                    ]
                , Html.label
                    [ Attributes.class "radio" ]
                    [ Html.input
                        [ Attributes.type_ "radio"
                        , Attributes.name "displayCondition"
                        , Attributes.checked (displayCondition == OnDemand)
                        , Events.onClick (DisplayConditionSelected OnDemand)
                        ]
                        []
                    , Html.text " On demand"
                    ]
                ]
            ]
        , case displayCondition of
            MatchingQuery count ->
                Html.div
                    [ Attributes.class "field" ]
                    [ Html.div
                        [ Attributes.class "control" ]
                        [ Html.input
                            [ Attributes.class "input"
                            , Attributes.type_ "number"
                            , Attributes.min "0"
                            , Attributes.step "1"
                            , Attributes.value (String.fromInt count)
                            , Events.onInput MatchingQueryCountChanged
                            ]
                            []
                        ]
                    ]

            _ ->
                Html.text ""
        ]


comboBoxSharedConfig : ComboBox.Shared String
comboBoxSharedConfig =
    { uniqueId = identity
    , matchesQuery = matchesQuery
    , printEntry = identity
    }


updateConfig : Bool -> Bool -> Bool -> Bool -> Bool -> SelectedDisplayCondition -> ComboBox.UpdateConfig String
updateConfig jumpAtEnds closeAfterMouseSelection separateFocus selectionFollowsFocus handleHomeAndEnd displayCondition =
    ComboBox.updateConfig comboBoxSharedConfig
        { jumpAtEnds = jumpAtEnds
        , closeAfterMouseSelection = closeAfterMouseSelection
        , separateFocus = separateFocus
        , selectionFollowsFocus = selectionFollowsFocus
        , handleHomeAndEnd = handleHomeAndEnd
        , displayCondition =
            case displayCondition of
                MatchingQuery count ->
                    ComboBox.matchingQuery count

                OnFocus ->
                    ComboBox.onFocus

                OnDemand ->
                    ComboBox.onDemand
        }


viewConfig : ComboBox.ViewConfig String String
viewConfig =
    ComboBox.viewConfig comboBoxSharedConfig
        { container = []
        , placeholder = "Select a locale..."
        , textfield =
            \{ maybeSelection, open } -> [ Attributes.class "textfield" ]
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
