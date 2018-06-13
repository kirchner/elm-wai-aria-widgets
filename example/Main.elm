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
import Json.Decode as Decode
import Rearrangable.SingleSelect
import Set exposing (Set)
import Widget exposing (HtmlDetails)
import Widget.Accordion as Accordion exposing (Accordion, PanelState(..))
import Widget.ComboBox as ComboBox exposing (ComboBox)
import Widget.Listbox as Listbox exposing (Entry, Listbox)
import Widget.Listbox.Dropdown as Dropdown exposing (Dropdown)


main : Program {} Model Msg
main =
    Browser.embed
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL


type alias Model =
    { accordion : Accordion
    , accordionJumpAtEnds : Bool
    , accordionHandleHomeAndEnd : Bool
    , accordionHandlePageDownPageUp : Bool

    -- LISTBOX
    , selectedLocales : Set String
    , listbox : Listbox
    , listboxJumpAtEnds : Bool
    , listboxSeparateFocus : Bool
    , listboxSelectionFollowsFocus : Bool
    , listboxHandleHomeAndEnd : Bool
    , listboxTypeAhead : Bool

    -- MUPPETS LISTBOX
    , selectedMuppets : Set String
    , muppetsListbox : Listbox

    -- LISTBOX DROPDOWN
    , selectedLocale : Maybe String
    , dropdown : Dropdown
    , dropdownJumpAtEnds : Bool
    , dropdownCloseAfterMouseSelection : Bool
    , dropdownSeparateFocus : Bool
    , dropdownSelectionFollowsFocus : Bool
    , dropdownHandleHomeAndEnd : Bool
    , dropdownTypeAhead : Bool

    -- COMBOBOX
    , selectedLocale2 : Maybe String
    , comboBox : ComboBox
    , comboBoxJumpAtEnds : Bool
    , comboBoxCloseAfterMouseSelection : Bool
    , comboBoxSeparateFocus : Bool
    , comboBoxSelectionFollowsFocus : Bool
    , comboBoxHandleHomeAndEnd : Bool
    , comboBoxDisplayCondition : SelectedDisplayCondition

    -- LISTBOX EXAMPLES
    , rearrangableSingleSelect : Rearrangable.SingleSelect.Model
    }


type SelectedDisplayCondition
    = MatchingQuery Int
    | OnFocus
    | OnDemand


init _ =
    ( { accordion = Accordion.init
      , accordionJumpAtEnds = True
      , accordionHandleHomeAndEnd = True
      , accordionHandlePageDownPageUp = True
      , selectedLocales = Set.empty
      , listbox = Listbox.init
      , listboxJumpAtEnds = True
      , listboxSeparateFocus = True
      , listboxSelectionFollowsFocus = False
      , listboxHandleHomeAndEnd = True
      , listboxTypeAhead = True
      , selectedMuppets = Set.empty
      , muppetsListbox = Listbox.init
      , selectedLocale = Nothing
      , dropdown = Dropdown.closed
      , dropdownJumpAtEnds = True
      , dropdownCloseAfterMouseSelection = True
      , dropdownSeparateFocus = True
      , dropdownSelectionFollowsFocus = False
      , dropdownHandleHomeAndEnd = True
      , dropdownTypeAhead = True
      , selectedLocale2 = Nothing
      , comboBox = ComboBox.closed
      , comboBoxJumpAtEnds = True
      , comboBoxCloseAfterMouseSelection = True
      , comboBoxSeparateFocus = True
      , comboBoxSelectionFollowsFocus = False
      , comboBoxHandleHomeAndEnd = True
      , comboBoxDisplayCondition = MatchingQuery 3
      , rearrangableSingleSelect = Rearrangable.SingleSelect.init
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = AccordionMsg (Cmd Msg) Accordion
    | AccordionJumpAtEndsChecked Bool
    | AccordionHandleHomeAndEndChecked Bool
    | AccordionHandlePageDownPageUpChecked Bool
      -- LISTBOX
    | ListboxMsg (Listbox.Msg String)
    | ListboxJumpAtEndsChecked Bool
    | ListboxSeparateFocusChecked Bool
    | ListboxSelectionFollowsFocusChecked Bool
    | ListboxHandleHomeAndEndChecked Bool
    | ListboxTypeAheadChecked Bool
      -- MUPPETS LISTBOX
    | MuppetsListboxMsg (Listbox.Msg String)
      -- LISTBOX DROPDOWN
    | DropdownMsg (Dropdown.Msg String)
    | DropdownJumpAtEndsChecked Bool
    | DropdownCloseAfterMouseSelectionChecked Bool
    | DropdownSeparateFocusChecked Bool
    | DropdownSelectionFollowsFocusChecked Bool
    | DropdownHandleHomeAndEndChecked Bool
    | DropdownTypeAheadChecked Bool
      -- COMBOBOX
    | ComboBoxMsg (ComboBox.Msg String)
    | ComboBoxJumpAtEndsChecked Bool
    | ComboBoxCloseAfterMouseSelectionChecked Bool
    | ComboBoxSeparateFocusChecked Bool
    | ComboBoxSelectionFollowsFocusChecked Bool
    | ComboBoxHandleHomeAndEndChecked Bool
    | ComboBoxDisplayConditionSelected SelectedDisplayCondition
    | ComboBoxMatchingQueryCountChanged String
      -- SINGLE-SELECT LISTBOX
    | RearrangableSingleSelectMsg Rearrangable.SingleSelect.Msg


type OutMsg
    = EntrySelected String
    | EntriesSelected (List String)
    | EntryUnselected String
    | AllEntriesSelected
    | AllEntriesUnselected


update msg model =
    case msg of
        AccordionMsg cmd newAccordion ->
            ( { model | accordion = newAccordion }
            , cmd
            )

        AccordionJumpAtEndsChecked enabled ->
            ( { model | accordionJumpAtEnds = enabled }
            , Cmd.none
            )

        AccordionHandleHomeAndEndChecked enabled ->
            ( { model | accordionHandleHomeAndEnd = enabled }
            , Cmd.none
            )

        AccordionHandlePageDownPageUpChecked enabled ->
            ( { model | accordionHandlePageDownPageUp = enabled }
            , Cmd.none
            )

        -- LISTBOX
        ListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update
                        (listboxUpdateConfig
                            model.listboxJumpAtEnds
                            model.listboxSeparateFocus
                            model.listboxSelectionFollowsFocus
                            model.listboxHandleHomeAndEnd
                            model.listboxTypeAhead
                        )
                        [ Listbox.onEntrySelect EntrySelected
                        , Listbox.onEntriesSelect EntriesSelected
                        , Listbox.onEntryUnselect EntryUnselected
                        , Listbox.onAllEntriesSelect AllEntriesSelected
                        , Listbox.onAllEntriesUnselect AllEntriesUnselected
                        ]
                        model.listbox
                        locales
                        (Set.toList model.selectedLocales)
                        listboxMsg
            in
            ( { model
                | listbox = newListbox
                , selectedLocales =
                    case maybeOutMsg of
                        Nothing ->
                            model.selectedLocales

                        Just (EntrySelected locale) ->
                            Set.insert locale model.selectedLocales

                        Just (EntriesSelected newLocales) ->
                            Set.union
                                (Set.fromList newLocales)
                                model.selectedLocales

                        Just (EntryUnselected locale) ->
                            Set.remove locale model.selectedLocales

                        Just AllEntriesSelected ->
                            Set.fromList allLocales

                        Just AllEntriesUnselected ->
                            Set.empty
              }
            , Cmd.map ListboxMsg listboxCmd
            )

        ListboxJumpAtEndsChecked enabled ->
            ( { model | listboxJumpAtEnds = enabled }
            , Cmd.none
            )

        ListboxSeparateFocusChecked enabled ->
            ( { model | listboxSeparateFocus = enabled }
            , Cmd.none
            )

        ListboxSelectionFollowsFocusChecked enabled ->
            ( { model | listboxSelectionFollowsFocus = enabled }
            , Cmd.none
            )

        ListboxHandleHomeAndEndChecked enabled ->
            ( { model | listboxHandleHomeAndEnd = enabled }
            , Cmd.none
            )

        ListboxTypeAheadChecked enabled ->
            ( { model | listboxTypeAhead = enabled }
            , Cmd.none
            )

        -- MUPPETS LISTBOX
        MuppetsListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update
                        (listboxUpdateConfig True True False True True)
                        [ Listbox.onEntrySelect EntrySelected
                        , Listbox.onEntriesSelect EntriesSelected
                        , Listbox.onEntryUnselect EntryUnselected
                        , Listbox.onAllEntriesSelect AllEntriesSelected
                        , Listbox.onAllEntriesUnselect AllEntriesUnselected
                        ]
                        model.muppetsListbox
                        muppets
                        (Set.toList model.selectedMuppets)
                        listboxMsg
            in
            ( { model
                | muppetsListbox = newListbox
                , selectedMuppets =
                    case maybeOutMsg of
                        Nothing ->
                            model.selectedMuppets

                        Just (EntrySelected muppet) ->
                            Set.insert muppet model.selectedMuppets

                        Just (EntriesSelected newMuppets) ->
                            Set.union
                                (Set.fromList newMuppets)
                                model.selectedMuppets

                        Just (EntryUnselected muppet) ->
                            Set.remove muppet model.selectedMuppets

                        Just AllEntriesSelected ->
                            Set.fromList allMuppets

                        Just AllEntriesUnselected ->
                            Set.empty
              }
            , Cmd.map MuppetsListboxMsg listboxCmd
            )

        -- LISTBOX DROPDOWN
        DropdownMsg dropdownMsg ->
            let
                ( newDropdown, dropdownCmd, maybeOutMsg ) =
                    Dropdown.update
                        (dropdownUpdateConfig
                            model.dropdownJumpAtEnds
                            model.dropdownCloseAfterMouseSelection
                            model.dropdownSeparateFocus
                            model.dropdownSelectionFollowsFocus
                            model.dropdownHandleHomeAndEnd
                            model.dropdownTypeAhead
                        )
                        EntrySelected
                        model.dropdown
                        locales
                        model.selectedLocale
                        dropdownMsg
            in
            ( { model
                | dropdown = newDropdown
                , selectedLocale =
                    case maybeOutMsg of
                        Just (EntrySelected locale) ->
                            Just locale

                        _ ->
                            model.selectedLocale
              }
            , Cmd.map DropdownMsg dropdownCmd
            )

        DropdownJumpAtEndsChecked enabled ->
            ( { model | dropdownJumpAtEnds = enabled }
            , Cmd.none
            )

        DropdownCloseAfterMouseSelectionChecked enabled ->
            ( { model | dropdownCloseAfterMouseSelection = enabled }
            , Cmd.none
            )

        DropdownSeparateFocusChecked enabled ->
            ( { model | dropdownSeparateFocus = enabled }
            , Cmd.none
            )

        DropdownSelectionFollowsFocusChecked enabled ->
            ( { model | dropdownSelectionFollowsFocus = enabled }
            , Cmd.none
            )

        DropdownHandleHomeAndEndChecked enabled ->
            ( { model | dropdownHandleHomeAndEnd = enabled }
            , Cmd.none
            )

        DropdownTypeAheadChecked enabled ->
            ( { model | dropdownTypeAhead = enabled }
            , Cmd.none
            )

        -- COMBO BOX
        ComboBoxMsg comboBoxMsg ->
            let
                ( newComboBox, comboBoxCmd, maybeOutMsg ) =
                    ComboBox.update
                        (comboBoxUpdateConfig
                            model.comboBoxJumpAtEnds
                            model.comboBoxCloseAfterMouseSelection
                            model.comboBoxSeparateFocus
                            model.comboBoxSelectionFollowsFocus
                            model.comboBoxHandleHomeAndEnd
                            model.comboBoxDisplayCondition
                        )
                        EntrySelected
                        model.comboBox
                        locales
                        model.selectedLocale2
                        comboBoxMsg
            in
            ( { model
                | comboBox = newComboBox
                , selectedLocale2 =
                    case maybeOutMsg of
                        Just (EntrySelected locale) ->
                            Just locale

                        _ ->
                            model.selectedLocale2
              }
            , Cmd.map ComboBoxMsg comboBoxCmd
            )

        ComboBoxJumpAtEndsChecked enabled ->
            ( { model | comboBoxJumpAtEnds = enabled }
            , Cmd.none
            )

        ComboBoxCloseAfterMouseSelectionChecked enabled ->
            ( { model | comboBoxCloseAfterMouseSelection = enabled }
            , Cmd.none
            )

        ComboBoxSeparateFocusChecked enabled ->
            ( { model | comboBoxSeparateFocus = enabled }
            , Cmd.none
            )

        ComboBoxSelectionFollowsFocusChecked enabled ->
            ( { model | comboBoxSelectionFollowsFocus = enabled }
            , Cmd.none
            )

        ComboBoxHandleHomeAndEndChecked enabled ->
            ( { model | comboBoxHandleHomeAndEnd = enabled }
            , Cmd.none
            )

        ComboBoxDisplayConditionSelected selectedDisplayCondition ->
            ( { model | comboBoxDisplayCondition = selectedDisplayCondition }
            , Cmd.none
            )

        ComboBoxMatchingQueryCountChanged rawCount ->
            case model.comboBoxDisplayCondition of
                MatchingQuery _ ->
                    case String.toInt rawCount of
                        Nothing ->
                            ( model, Cmd.none )

                        Just count ->
                            ( { model | comboBoxDisplayCondition = MatchingQuery count }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        -- SINGLE-SELECT LISTBOX
        RearrangableSingleSelectMsg subMsg ->
            let
                ( newState, subCmd ) =
                    Rearrangable.SingleSelect.update subMsg model.rearrangableSingleSelect
            in
            ( { model | rearrangableSingleSelect = newState }
            , Cmd.map RearrangableSingleSelectMsg subCmd
            )



---- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ Sub.map ListboxMsg (Listbox.subscriptions model.listbox)
        , Sub.map MuppetsListboxMsg (Listbox.subscriptions model.muppetsListbox)
        , Sub.map DropdownMsg (Dropdown.subscriptions model.dropdown)
        , Sub.map ComboBoxMsg (ComboBox.subscriptions model.comboBox)
        , Sub.map RearrangableSingleSelectMsg
            (Rearrangable.SingleSelect.subscriptions model.rearrangableSingleSelect)
        ]



---- VIEW


view model =
    Html.section
        [ Attributes.class "section" ]
        [ Html.div
            [ Attributes.class "container" ]
            [ Accordion.view
                (accordionViewConfig
                    model.accordionJumpAtEnds
                    model.accordionHandleHomeAndEnd
                    model.accordionHandlePageDownPageUp
                )
                AccordionMsg
                "examples"
                model.accordion
                [ Accordion.section Expanded
                    { id = "accordion"
                    , header = "Accordion"
                    , panel =
                        [ Html.lazy3 viewAccordionConfiguration
                            model.accordionJumpAtEnds
                            model.accordionHandleHomeAndEnd
                            model.accordionHandlePageDownPageUp
                        ]
                    }
                , Accordion.section Collapsed
                    { id = "listboxes"
                    , header = "Listboxes"
                    , panel =
                        [ Html.form
                            [ Attributes.style "width" "100%" ]
                            [ Html.lazy2 viewListbox
                                model.listbox
                                model.selectedLocales
                            , Html.lazy5 viewListboxConfiguration
                                model.listboxJumpAtEnds
                                model.listboxSeparateFocus
                                model.listboxSelectionFollowsFocus
                                model.listboxHandleHomeAndEnd
                                model.listboxTypeAhead
                            ]
                        ]
                    }
                , Accordion.section Collapsed
                    { id = "muppets"
                    , header = "Muppets"
                    , panel =
                        [ Html.form
                            [ Attributes.style "width" "100%" ]
                            [ Html.lazy2 viewMuppetsListbox
                                model.muppetsListbox
                                model.selectedMuppets
                            ]
                        ]
                    }
                , Accordion.section Collapsed
                    { id = "dropdown-menus"
                    , header = "Dropdown Menus"
                    , panel =
                        [ Html.form
                            [ Attributes.style "width" "100%" ]
                            [ Html.div
                                [ Attributes.class "columns" ]
                                [ Html.div
                                    [ Attributes.class "column" ]
                                    [ Html.lazy2 viewDropdown model.dropdown model.selectedLocale
                                    , Html.lazy6 viewDropdownConfiguration
                                        model.dropdownJumpAtEnds
                                        model.dropdownCloseAfterMouseSelection
                                        model.dropdownSeparateFocus
                                        model.dropdownSelectionFollowsFocus
                                        model.dropdownHandleHomeAndEnd
                                        model.dropdownTypeAhead
                                    ]
                                , Html.div
                                    [ Attributes.class "column" ]
                                    [ Html.lazy2 viewComboBox model.comboBox model.selectedLocale2
                                    , Html.lazy6 viewComboBoxConfiguration
                                        model.comboBoxJumpAtEnds
                                        model.comboBoxCloseAfterMouseSelection
                                        model.comboBoxSeparateFocus
                                        model.comboBoxSelectionFollowsFocus
                                        model.comboBoxHandleHomeAndEnd
                                        model.comboBoxDisplayCondition
                                    ]
                                ]
                            ]
                        ]
                    }
                , Accordion.section Expanded
                    { id = "single-select-listbox"
                    , header = "Single-Select Listbox"
                    , panel =
                        [ Html.p
                            [ Attributes.style "margin-bottom" "20px" ]
                            [ Html.text "This is a reimplementation of "
                            , Html.a
                                [ Attributes.href "https://www.w3.org/TR/2017/NOTE-wai-aria-practices-1.1-20171214/examples/listbox/listbox-rearrangeable.html" ]
                                [ Html.text "Example 1" ]
                            , Html.text " of the Example Listboxes with Rearrangeable Options of the WAI-ARIA Authoring Practices 1.1."
                            ]
                        , Rearrangable.SingleSelect.view model.rearrangableSingleSelect
                            |> Html.map RearrangableSingleSelectMsg
                        ]
                    }
                ]
            ]
        ]


viewAccordionConfiguration accordionJumpAtEnds accordionHandleHomeAndEnd accordionHandlePageDownPageUp =
    Html.form
        [ Attributes.style "width" "100%" ]
        [ Html.label
            [ Attributes.class "label" ]
            [ Html.text "Configuration" ]
        , viewCheckbox AccordionJumpAtEndsChecked
            accordionJumpAtEnds
            "Jump at ends"
        , viewCheckbox AccordionHandleHomeAndEndChecked
            accordionHandleHomeAndEnd
            "Handle Home and End keys"
        , viewCheckbox AccordionHandlePageDownPageUpChecked
            accordionHandlePageDownPageUp
            "Handle Page Down and Page Up keys"
        ]


viewListbox listbox selection =
    Html.div
        [ Attributes.class "field" ]
        [ Html.label
            [ Attributes.id "locales-label" ]
            [ Html.text "Locale" ]
        , Html.div
            [ Attributes.class "control" ]
            [ selection
                |> Set.toList
                |> Listbox.viewLazy
                    (\_ -> 42)
                    (\_ -> 31)
                    listboxViewConfig
                    { id = "locales"
                    , labelledBy = "locales-label"
                    }
                    listbox
                    locales
                |> Html.map ListboxMsg
            ]
        , Html.p
            [ Attributes.class "help" ]
            [ Html.text <|
                if Set.isEmpty selection then
                    "nothing selected"
                else
                    "currently selected: "
                        ++ (selection
                                |> Set.toList
                                |> String.join ", "
                           )
            ]
        ]


viewListboxConfiguration listboxJumpAtEnds listboxSeparateFocus listboxSelectionFollowsFocus listboxHandleHomeAndEnd listboxTypeAhead =
    Html.div []
        [ Html.label
            [ Attributes.class "label" ]
            [ Html.text "Configuration" ]
        , viewCheckbox ListboxJumpAtEndsChecked
            listboxJumpAtEnds
            "Jump at ends"
        , viewCheckbox ListboxSeparateFocusChecked
            listboxSeparateFocus
            "Separate focus for mouse and keyboard"
        , viewCheckbox ListboxSelectionFollowsFocusChecked
            listboxSelectionFollowsFocus
            "Selection follows focus"
        , viewCheckbox ListboxHandleHomeAndEndChecked
            listboxHandleHomeAndEnd
            "Handle Home and End keys"
        , viewCheckbox ListboxTypeAheadChecked
            listboxTypeAhead
            "Type ahead"
        ]


viewMuppetsListbox listbox selection =
    Html.div
        [ Attributes.class "field" ]
        [ Html.label
            [ Attributes.id "muppets-label" ]
            [ Html.text "Muppets" ]
        , Html.div
            [ Attributes.class "control" ]
            [ selection
                |> Set.toList
                |> Listbox.viewLazy
                    (\_ -> 42)
                    (\_ -> 42)
                    listboxViewConfig
                    { id = "muppets"
                    , labelledBy = "muppets-label"
                    }
                    listbox
                    muppets
                |> Html.map MuppetsListboxMsg
            ]
        , Html.p
            [ Attributes.class "help" ]
            [ Html.text <|
                if Set.isEmpty selection then
                    "nothing selected"
                else
                    "currently selected: "
                        ++ (selection
                                |> Set.toList
                                |> String.join ", "
                           )
            ]
        ]


viewDropdown dropdown selection =
    Html.div
        [ Attributes.class "field" ]
        [ Html.label
            [ Attributes.id "locales-dropdown-label" ]
            [ Html.text "Locale" ]
        , Html.div
            [ Attributes.class "control" ]
            [ selection
                |> Dropdown.view dropdownViewConfig
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


viewDropdownConfiguration dropdownJumpAtEnds dropdownCloseAfterMouseSelection dropdownSeparateFocus dropdownSelectionFollowsFocus dropdownHandleHomeAndEnd dropdownTypeAhead =
    Html.div []
        [ Html.label
            [ Attributes.class "label" ]
            [ Html.text "Configuration" ]
        , viewCheckbox DropdownJumpAtEndsChecked
            dropdownJumpAtEnds
            "Jump at ends"
        , viewCheckbox DropdownCloseAfterMouseSelectionChecked
            dropdownCloseAfterMouseSelection
            "Close after mouse selection"
        , viewCheckbox DropdownSeparateFocusChecked
            dropdownSeparateFocus
            "Separate focus for mouse and keyboard"
        , viewCheckbox DropdownSelectionFollowsFocusChecked
            dropdownSelectionFollowsFocus
            "Selection follows focus"
        , viewCheckbox DropdownHandleHomeAndEndChecked
            dropdownHandleHomeAndEnd
            "Handle Home and End keys"
        , viewCheckbox DropdownTypeAheadChecked
            dropdownTypeAhead
            "Type ahead"
        ]


viewComboBox comboBox selection =
    Html.div
        [ Attributes.class "field" ]
        [ Html.label
            [ Attributes.id "locales-combo-box-label" ]
            [ Html.text "Locale" ]
        , Html.div
            [ Attributes.class "control" ]
            [ selection
                |> ComboBox.view comboBoxViewConfig
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


viewComboBoxConfiguration comboBoxJumpAtEnds comboBoxCloseAfterMouseSelection comboBoxSeparateFocus comboBoxSelectionFollowsFocus comboBoxHandleHomeAndEnd comboBoxDisplayCondition =
    Html.div []
        [ Html.label
            [ Attributes.class "label" ]
            [ Html.text "Configuration" ]
        , viewCheckbox ComboBoxJumpAtEndsChecked
            comboBoxJumpAtEnds
            "Jump at ends"
        , viewCheckbox ComboBoxCloseAfterMouseSelectionChecked
            comboBoxCloseAfterMouseSelection
            "Close after mouse selection"
        , viewCheckbox ComboBoxSeparateFocusChecked
            comboBoxSeparateFocus
            "Separate focus for mouse and keyboard"
        , viewCheckbox ComboBoxSelectionFollowsFocusChecked
            comboBoxSelectionFollowsFocus
            "Selection follows focus"
        , viewCheckbox ComboBoxHandleHomeAndEndChecked
            comboBoxHandleHomeAndEnd
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
                            case comboBoxDisplayCondition of
                                MatchingQuery _ ->
                                    True

                                _ ->
                                    False
                        , case comboBoxDisplayCondition of
                            MatchingQuery count ->
                                Events.onClick
                                    (ComboBoxDisplayConditionSelected (MatchingQuery count))

                            _ ->
                                Events.onClick
                                    (ComboBoxDisplayConditionSelected (MatchingQuery 3))
                        ]
                        []
                    , Html.text " Matching query"
                    ]
                , Html.label
                    [ Attributes.class "radio" ]
                    [ Html.input
                        [ Attributes.type_ "radio"
                        , Attributes.name "displayCondition"
                        , Attributes.checked
                            (comboBoxDisplayCondition == OnFocus)
                        , Events.onClick
                            (ComboBoxDisplayConditionSelected OnFocus)
                        ]
                        []
                    , Html.text " On focus"
                    ]
                , Html.label
                    [ Attributes.class "radio" ]
                    [ Html.input
                        [ Attributes.type_ "radio"
                        , Attributes.name "displayCondition"
                        , Attributes.checked
                            (comboBoxDisplayCondition == OnDemand)
                        , Events.onClick
                            (ComboBoxDisplayConditionSelected OnDemand)
                        ]
                        []
                    , Html.text " On demand"
                    ]
                ]
            ]
        , case comboBoxDisplayCondition of
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
                            , Attributes.value
                                (String.fromInt count)
                            , Events.onInput
                                ComboBoxMatchingQueryCountChanged
                            ]
                            []
                        ]
                    ]

            _ ->
                Html.text ""
        ]


viewCheckbox checked enabled description =
    Html.div
        [ Attributes.class "field" ]
        [ Html.div
            [ Attributes.class "control" ]
            [ Html.label
                [ Attributes.class "checkbox" ]
                [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.checked enabled
                    , Events.onCheck checked
                    ]
                    []
                , Html.text (" " ++ description)
                ]
            ]
        ]



---- CONFIG


accordionViewConfig : Bool -> Bool -> Bool -> Accordion.ViewConfig String
accordionViewConfig jumpAtEnds handleHomeAndEnd handlePageDownPageUp =
    Accordion.viewConfig
        { jumpAtEnds = jumpAtEnds
        , handleHomeAndEnd = handleHomeAndEnd
        , handlePageDownPageUp = handlePageDownPageUp
        , collapsedCount = Accordion.AnyNumber
        , expandedCount = Accordion.AnyNumber
        }
        { dl = [ Attributes.class "panel" ]
        , dt = [ Attributes.class "panel-heading" ]
        , button =
            \panelState label ->
                { attributes = [ Attributes.class "accordion-button" ]
                , children =
                    [ Html.span [] [ Html.text label ]
                    , Html.span
                        [ Attributes.class "icon"
                        , Attributes.style "float" "right"
                        ]
                        [ Html.i
                            [ Attributes.class "fas"
                            , Attributes.class <|
                                case panelState of
                                    Collapsed ->
                                        "fa-angle-down"

                                    Expanded ->
                                        "fa-angle-up"
                            ]
                            []
                        ]
                    ]
                }
        , dd =
            [ Attributes.class "panel-block"
            , Attributes.style "display" "block"
            ]
        }


listboxUpdateConfig : Bool -> Bool -> Bool -> Bool -> Bool -> Listbox.UpdateConfig String
listboxUpdateConfig jumpAtEnds separateFocus selectionFollowsFocus handleHomeAndEnd typeAhead =
    Listbox.updateConfig identity
        { jumpAtEnds = jumpAtEnds
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


listboxViewConfig : Listbox.ViewConfig String String
listboxViewConfig =
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
        , liDivider =
            \text ->
                { attributes =
                    [ Attributes.class "divider" ]
                , children = [ Html.text text ]
                }
        , empty = Html.div [] [ Html.text "this list is empty" ]
        , focusable = True
        }


dropdownUpdateConfig : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Dropdown.UpdateConfig String
dropdownUpdateConfig jumpAtEnds closeAfterMouseSelection separateFocus selectionFollowsFocus handleHomeAndEnd typeAhead =
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


dropdownViewConfig : Dropdown.ViewConfig String String
dropdownViewConfig =
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


comboBoxSharedConfig : ComboBox.Shared String
comboBoxSharedConfig =
    { uniqueId = identity
    , matchesQuery = matchesQuery
    , printEntry = identity
    }


comboBoxUpdateConfig : Bool -> Bool -> Bool -> Bool -> Bool -> SelectedDisplayCondition -> ComboBox.UpdateConfig String
comboBoxUpdateConfig jumpAtEnds closeAfterMouseSelection separateFocus selectionFollowsFocus handleHomeAndEnd displayCondition =
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


comboBoxViewConfig : ComboBox.ViewConfig String String
comboBoxViewConfig =
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


matchesQuery : String -> String -> Bool
matchesQuery query value =
    String.toLower value
        |> String.contains (String.toLower query)


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


muppets : List (Listbox.Entry String String)
muppets =
    List.concat
        [ [ Listbox.divider "Main character" ]
        , List.map Listbox.option mainCharacters
        , [ Listbox.divider "Supporting characters" ]
        , List.map Listbox.option supportingCharacters
        , [ Listbox.divider "Minor characters" ]
        , List.map Listbox.option minorCharacters
        ]


allMuppets : List String
allMuppets =
    List.concat
        [ mainCharacters
        , supportingCharacters
        , minorCharacters
        ]


mainCharacters : List String
mainCharacters =
    [ "Kermit the Frog"
    , "Miss Piggy"
    , "Fozzie Bear"
    , "Gonzo"
    , "Rowlf the Dog"
    , "Scooter"
    , "Pepe the King Prawn"
    , "Rizzo the Rat"
    , "Animal"
    , "Walter"
    ]


supportingCharacters : List String
supportingCharacters =
    [ "Bunsen Honeydew"
    , "Beaker"
    , "Sam Eagle"
    , "The Swedish Chef"
    , "Dr. Teeth and The Electric Mayhem"
    , "Statler and Waldorf"
    , "Camilla the Chicken"
    , "Bobo the Bear"
    , "Clifford"
    ]


minorCharacters : List String
minorCharacters =
    [ "'80s Robot"
    , "Andy and Randy Pig"
    , "Bean Bunny"
    , "Beauregard"
    , "Constantine"
    , "Crazy Harry"
    , "Johnny Fiama and Sal Minella"
    , "Lew Zealand"
    , "Link Hogthrob"
    , "Marvin Suggs"
    , "The Muppet Newsman"
    , "Pops"
    , "Robin the Frog"
    , "Sweetums"
    , "Uncle Deadly"
    ]


locales : List (Listbox.Entry String String)
locales =
    List.map Listbox.option allLocales


allLocales : List String
allLocales =
    [ "Abkhazian"
    , "Achinese"
    , "Acoli"
    , "Adangme"
    , "Adyghe"
    , "Afar"
    , "Afar, Djibouti"
    , "Afar, Eritrea"
    , "Afar, Ethiopia"
    , "Afar(Ethiopic)"
    , "Afrihili"
    , "Afrikaans"
    , "Afrikaans, Namibia"
    , "Afrikaans, South Africa"
    , "Afro-Asiatic Language"
    , "Ainu"
    , "Ainu(Latin)"
    , "Akan"
    , "Akan, Ghana"
    , "Akkadian"
    , "Albanian"
    , "Albanian, Albania"
    , "Aleut"
    , "Algonquian Language"
    , "Altaic Language"
    , "Amharic"
    , "Amharic, Ethiopia"
    , "Angika"
    , "Apache Language"
    , "Arabic"
    , "Arabic, Algeria"
    , "Arabic(Perso-Arabic)"
    , "Arabic, Bahrain"
    , "Arabic, Egypt"
    , "Arabic, Iraq"
    , "Arabic, Jordan"
    , "Arabic, Kuwait"
    , "Arabic, Lebanon"
    , "Arabic, Libya"
    , "Arabic, Morocco"
    , "Arabic, Oman"
    , "Arabic, Qatar"
    , "Arabic, Saudi Arabia"
    , "Arabic, Sudan"
    , "Arabic, Syria"
    , "Arabic, Tunisia"
    , "Arabic, United Arab Emirates"
    , "Arabic, Yemen"
    , "Aragonese"
    , "Arapaho"
    , "Araucanian"
    , "Arawak"
    , "Armenian"
    , "Armenian, Armenia"
    , "Aromanian"
    , "Aromanian(Greek)"
    , "Aromanian(Latin)"
    , "Artificial Language"
    , "Assamese"
    , "Assamese, India"
    , "Asturian"
    , "Athapascan Language"
    , "Atsam"
    , "Atsam, Nigeria"
    , "Australian Language"
    , "Austronesian Language"
    , "Avaric"
    , "Avestan"
    , "Awadhi"
    , "Aymara"
    , "Azeri"
    , "Azeri(Perso-Arabic)"
    , "Azeri, Azerbaijan"
    , "Azeri, Azerbaijan(Cyrillic)"
    , "Azeri, Azerbaijan(Latin)"
    , "Azeri(Cyrillic)"
    , "Azeri(Latin)"
    , "Azeri, Iran"
    , "Azeri, Iran(Perso-Arabic)"
    , "Balinese"
    , "Baltic Language"
    , "Baluchi"
    , "Baluchi(Perso-Arabic)"
    , "Bambara"
    , "Bamileke Language"
    , "Banda"
    , "Bantu"
    , "Basa"
    , "Bashkir"
    , "Basque"
    , "Basque, France"
    , "Basque, Spain"
    , "Batak"
    , "Beja"
    , "Belarusian"
    , "Belarusian, Belarus"
    , "Belarusian(Cyrillic)"
    , "Belarusian(Latin)"
    , "Bemba"
    , "bengali"
    , "bengali, bangladesh"
    , "Bengali, India"
    , "Berber"
    , "Bhojpuri"
    , "Bihari"
    , "Bikol"
    , "Bini"
    , "Bislama"
    , "Blin"
    , "Blin, Eritrea"
    , "Blissymbols"
    , "Bosnian"
    , "Bosnian, Bosnia and Herzegovina"
    , "Braj"
    , "Breton"
    , "Buginese"
    , "Bulgarian"
    , "Bulgarian, Bulgaria"
    , "Buriat"
    , "Burmese"
    , "Burmese, Myanmar [Burma]"
    , "Caddo"
    , "Carib"
    , "Catalan"
    , "Catalan, Spain"
    , "Caucasian Language"
    , "Cebuano"
    , "Celtic Language"
    , "Central American Indian Language"
    , "Khmer"
    , "Khmer, Cambodia"
    , "Chamic Language"
    , "Chamic Language(Perso-Arabic)"
    , "Chamorro"
    , "Chechen"
    , "Cherokee"
    , "Cheyenne"
    , "Nyanja"
    , "Nyanja, Malawi"
    , "Chinese"
    , "Chinese, China"
    , "Chinese, China(Simplified Han)"
    , "Chinese, Hong Kong"
    , "Chinese, Hong Kong(Simplified Han)"
    , "Chinese, Hong Kong(Traditional Han)"
    , "Chinese, Macau"
    , "Chinese, Macau(Simplified Han)"
    , "Chinese, Macau(Traditional Han)"
    , "Chinese(Simplified Han)"
    , "Chinese, Singapore"
    , "Chinese, Singapore(Simplified Han)"
    , "Chinese, Taiwan"
    , "Chinese, Taiwan(Traditional Han)"
    , "Chinese(Traditional Han)"
    , "Chinook Jargon"
    , "Chipewyan"
    , "Choctaw"
    , "Church Slavic"
    , "Chuukese"
    , "Chuvash"
    , "Cornish"
    , "Cornish, United Kingdom"
    , "Corsican"
    , "Cree"
    , "Creek"
    , "Creole or Pidgin"
    , "Crimean Turkish"
    , "Crimean Turkish(Cyrillic)"
    , "Crimean Turkish(Latin)"
    , "Croatian"
    , "Croatian, Croatia"
    , "Cushitic Language"
    , "Czech"
    , "Czech, Czech Republic"
    , "Dakota"
    , "Danish"
    , "Danish, Denmark"
    , "Dargwa"
    , "Dayak"
    , "Delaware"
    , "Dinka"
    , "Divehi"
    , "Divehi, Maldives"
    , "Divehi(Thaana)"
    , "Dogri"
    , "Dogrib"
    , "Dravidian Language"
    , "Duala"
    , "Dutch"
    , "Dutch, Belgium"
    , "Dutch, Netherlands"
    , "Dyula"
    , "Dzongkha"
    , "Dzongkha, Bhutan"
    , "Eastern Frisian"
    , "Efik"
    , "Ekajuk"
    , "English"
    , "English, American Samoa"
    , "English, Australia"
    , "English-based Creole or Pidgin"
    , "English, Belgium"
    , "English, Belize"
    , "English, Botswana"
    , "English, Canada"
    , "English(Deseret)"
    , "English, Guam"
    , "English, Hong Kong"
    , "English, India"
    , "English, Ireland"
    , "English, Israel"
    , "English, Jamaica"
    , "English, Malta"
    , "English, Marshall Islands"
    , "English, Namibia"
    , "English, New Zealand"
    , "English, Northern Mariana Islands"
    , "English, Pakistan"
    , "English, Philippines"
    , "English(Shavian)"
    , "English, Singapore"
    , "English, South Africa"
    , "English, Trinidad and Tobago"
    , "English, United Kingdom"
    , "English, United States"
    , "English, United States(Deseret)"
    , "English, U.S. Minor Outlying Islands"
    , "English, U.S. Virgin Islands"
    , "English, Zimbabwe"
    , "Erzya"
    , "Esperanto"
    , "Estonian"
    , "Estonian, Estonia"
    , "Ewe"
    , "Ewe, Ghana"
    , "Ewe, Togo"
    , "Ewondo"
    , "Fang"
    , "Fanti"
    , "Faroese"
    , "Faroese, Faroe Islands"
    , "Fijian"
    , "Filipino"
    , "Filipino, Philippines"
    , "Finnish"
    , "Finnish, Finland"
    , "Finno-Ugrian Language"
    , "Fon"
    , "French"
    , "French-based Creole or Pidgin"
    , "French, Belgium"
    , "French, Canada"
    , "French, France"
    , "French, Luxembourg"
    , "French, Monaco"
    , "French, Senegal"
    , "French, Switzerland"
    , "French, Morocco"
    , "Friulian"
    , "Friulian, Italy"
    , "Fulah"
    , "Fulah(Perso-Arabic)"
    , "Fulah(Latin)"
    , "Ga"
    , "Scottish Gaelic"
    , "Ga, Ghana"
    , "Galician"
    , "Galician, Spain"
    , "Ganda"
    , "Gayo"
    , "Gbaya"
    , "Geez"
    , "Geez, Eritrea"
    , "Geez, Ethiopia"
    , "Georgian"
    , "Georgian, Georgia"
    , "German"
    , "German, Austria"
    , "German, Belgium"
    , "German, Germany"
    , "Germanic Language"
    , "German, Liechtenstein"
    , "German, Luxembourg"
    , "German, Switzerland"
    , "Gilbertese"
    , "Gondi"
    , "Gorontalo"
    , "Grebo"
    , "Greek"
    , "Greek, Cyprus"
    , "Greek, Greece"
    , "Guarani"
    , "Gujarati"
    , "Gujarati, India"
    , "Gwichʼin"
    , "Haida"
    , "Haitian"
    , "Hausa"
    , "Hausa(Perso-Arabic)"
    , "Hausa, Ghana"
    , "Hausa, Ghana(Latin)"
    , "Hausa(Latin)"
    , "Hausa, Niger"
    , "Hausa, Nigeria"
    , "Hausa, Nigeria(Perso-Arabic)"
    , "Hausa, Nigeria(Latin)"
    , "Hausa, Niger(Latin)"
    , "Hausa, Sudan"
    , "Hausa, Sudan(Perso-Arabic)"
    , "Hawaiian"
    , "Hawaiian, United States"
    , "Hebrew"
    , "Hebrew(Hebrew)"
    , "Hebrew, Israel"
    , "Herero"
    , "Hiligaynon"
    , "Himachali"
    , "Hindi"
    , "Hindi, India"
    , "Hiri Motu"
    , "Hittite"
    , "Hmong"
    , "Hungarian"
    , "Hungarian, Hungary"
    , "Hupa"
    , "Iban"
    , "Icelandic"
    , "Icelandic, Iceland"
    , "Ido"
    , "Igbo"
    , "Igbo, Nigeria"
    , "Ijo"
    , "Iloko"
    , "Inari Sami"
    , "Indic Language"
    , "Indo-European Language"
    , "Indonesian"
    , "Indonesian(Perso-Arabic)"
    , "Indonesian, Indonesia"
    , "Indonesian, Indonesia(Perso-Arabic)"
    , "Ingush"
    , "Interlingua"
    , "Interlingue"
    , "Inuktitut"
    , "Inuktitut, Canada"
    , "Inupiaq"
    , "Iranian Language"
    , "Irish"
    , "Irish, Ireland"
    , "Iroquoian Language"
    , "Italian"
    , "Italian, Italy"
    , "Italian, Switzerland"
    , "Japanese"
    , "Japanese, Japan"
    , "Javanese"
    , "Javanese(Javanese)"
    , "Javanese(Latin)"
    , "Judeo-Arabic"
    , "Judeo-Persian"
    , "Kabardian"
    , "Kabyle"
    , "Kachin"
    , "Kalaallisut"
    , "Kalaallisut, Greenland"
    , "Kalmyk"
    , "Kalmyk(Cyrillic)"
    , "Kalmyk(Mongolian)"
    , "Kamba"
    , "Kamba, Kenya"
    , "Kannada"
    , "Kannada, India"
    , "Kanuri"
    , "Karachay-Balkar"
    , "Kara-Kalpak"
    , "Karelian"
    , "Karen"
    , "Kashmiri"
    , "Kashmiri(Perso-Arabic)"
    , "Kashmiri(Devanagari)"
    , "Kashmiri(Latin)"
    , "Kashubian"
    , "Kawi"
    , "Kazakh"
    , "Kazakh(Perso-Arabic)"
    , "Kazakh(Cyrillic)"
    , "Kazakh, Kazakhstan"
    , "Kazakh, Kazakhstan(Perso-Arabic)"
    , "Kazakh, Kazakhstan(Cyrillic)"
    , "Kazakh, Kazakhstan(Latin)"
    , "Kazakh(Latin)"
    , "Khasi"
    , "Khoisan Language"
    , "Khotanese"
    , "Kikuyu"
    , "Kimbundu"
    , "Kinyarwanda"
    , "Kinyarwanda, Rwanda"
    , "Kirghiz(Cyrillic)"
    , "Kirghiz"
    , "Kirghiz(Perso-Arabic)"
    , "Kirghiz, Kyrgyzstan"
    , "Kirghiz(Latin)"
    , "Klingon"
    , "Komi"
    , "Kongo"
    , "Konkani"
    , "Konkani, India"
    , "Konkani, India(Kannada)"
    , "Konkani, India(Latin)"
    , "Konkani, India(Malayalam)"
    , "Konkani(Kannada)"
    , "Konkani(Latin)"
    , "Konkani(Malayalam)"
    , "Korean"
    , "Korean, South Korea"
    , "Koro"
    , "Koro, Ivory Coast"
    , "Kosraean"
    , "Kpelle"
    , "Kpelle, Guinea"
    , "Kpelle, Liberia"
    , "Kru"
    , "Kuanyama"
    , "Kumyk"
    , "Kurdish"
    , "Kurdish(Perso-Arabic)"
    , "Kurdish, Iran"
    , "Kurdish, Iran(Perso-Arabic)"
    , "Kurdish, Iraq"
    , "Kurdish, Iraq(Perso-Arabic)"
    , "Kurdish(Latin)"
    , "Kurdish, Syria"
    , "Kurdish, Syria(Perso-Arabic)"
    , "Kurdish, Turkey"
    , "Kurdish, Turkey(Latin)"
    , "Kurukh"
    , "Kutenai"
    , "Ladino"
    , "Ladino(Hebrew)"
    , "Ladino(Latin)"
    , "Lahnda"
    , "Lamba"
    , "Lao"
    , "Lao, Laos"
    , "Latin"
    , "Latvian"
    , "Latvian, Latvia"
    , "Lezghian"
    , "Limburgish"
    , "Lingala"
    , "Lingala, Congo [Republic]"
    , "Lingala, Congo [DRC]"
    , "Lithuanian"
    , "Lithuanian, Lithuania"
    , "Lojban"
    , "Lower Sorbian"
    , "Low German"
    , "Low German, Germany"
    , "Lozi"
    , "Luba-Katanga"
    , "Luba-Lulua"
    , "Luiseno"
    , "Lule Sami"
    , "Lunda"
    , "Luo"
    , "Lushai"
    , "Luxembourgish"
    , "Macedonian"
    , "Macedonian, Macedonia [FYROM]"
    , "Madurese"
    , "Magahi"
    , "Maithili"
    , "Makasar"
    , "Makasar(Buginese)"
    , "Makasar(Latin)"
    , "Malagasy"
    , "Malay"
    , "Malayalam"
    , "Malayalam(Perso-Arabic)"
    , "Malayalam, India"
    , "Malayalam, India(Perso-Arabic)"
    , "Malayalam, India(Malayalam)"
    , "Malayalam(Malayalam)"
    , "Malay(Perso-Arabic)"
    , "Malay, Brunei"
    , "Malay, Brunei(Latin)"
    , "Malay(Latin)"
    , "Malay, Malaysia"
    , "Malay, Malaysia(Latin)"
    , "Maltese"
    , "Maltese, Malta"
    , "Manchu"
    , "Mandar"
    , "Mandingo"
    , "Manipuri"
    , "Manobo Language"
    , "Manx"
    , "Manx, United Kingdom"
    , "Maori"
    , "Marathi"
    , "Marathi, India"
    , "Mari"
    , "Marshallese"
    , "Marwari"
    , "Masai"
    , "Mayan Language"
    , "Mende"
    , "Micmac"
    , "Minangkabau"
    , "Mirandese"
    , "Mohawk"
    , "Moksha"
    , "Moldavian"
    , "Moldavian, Moldova"
    , "Mongo"
    , "Mongolian"
    , "Mongolian, China"
    , "Mongolian, China(Mongolian)"
    , "Mongolian(Cyrillic)"
    , "Mongolian, Mongolia"
    , "Mongolian, Mongolia(Cyrillic)"
    , "Mongolian(Mongolian)"
    , "Mon-Khmer Language"
    , "Mossi"
    , "Multiple Languages"
    , "Munda Language"
    , "Nahuatl"
    , "Nauru"
    , "Navajo"
    , "North Ndebele"
    , "South Ndebele"
    , "South Ndebele, South Africa"
    , "Ndonga"
    , "Neapolitan"
    , "Newari"
    , "Nepali"
    , "Nepali, India"
    , "Nepali, Nepal"
    , "Nias"
    , "Niger-Kordofanian Language"
    , "Nilo-Saharan Language"
    , "Niuean"
    , "N’Ko"
    , "Nogai"
    , "No linguistic content"
    , "North American Indian Language"
    , "Northern Frisian"
    , "Northern Sami"
    , "Northern Sami, Finland"
    , "Northern Sami, Norway"
    , "Norwegian Bokmål"
    , "Norwegian Bokmål, Norway"
    , "Norwegian Nynorsk"
    , "Norwegian Nynorsk, Norway"
    , "Nubian Language"
    , "Nyamwezi"
    , "Nyankole"
    , "Nyoro"
    , "Nzima"
    , "Occitan"
    , "Occitan, France"
    , "Ojibwa"
    , "Oriya"
    , "Oriya, India"
    , "Oromo"
    , "Oromo, Ethiopia"
    , "Oromo, Kenya"
    , "Osage"
    , "Ossetic"
    , "Ossetic(Cyrillic)"
    , "Ossetic(Latin)"
    , "Otomian Language"
    , "Pahlavi"
    , "Palauan"
    , "Pali"
    , "Pali(Devanagari)"
    , "Pali(Sinhala)"
    , "Pali(Thai)"
    , "Pampanga"
    , "Pampanga, India"
    , "Pangasinan"
    , "Punjabi(Perso-Arabic)"
    , "Punjabi(Devanagari)"
    , "Punjabi(Gurmukhi)"
    , "Punjabi, India(Devanagari)"
    , "Punjabi, India(Gurmukhi)"
    , "Punjabi, Pakistan(Perso-Arabic)"
    , "Punjabi, Pakistan(Devanagari)"
    , "Papiamento"
    , "Papuan Language"
    , "Northern Sotho"
    , "Northern Sotho, South Africa"
    , "Persian"
    , "Persian, Afghanistan"
    , "Persian(Perso-Arabic)"
    , "Persian(Cyrillic)"
    , "Persian, Iran"
    , "Philippine Language"
    , "Pohnpeian"
    , "Polish"
    , "Polish, Poland"
    , "Portuguese"
    , "Portuguese-based Creole or Pidgin"
    , "Portuguese, Brazil"
    , "Portuguese, Portugal"
    , "Prakrit Language"
    , "Punjabi"
    , "Punjabi, Pakistan"
    , "Pushto"
    , "Pushto, Afghanistan"
    , "Pushto(Perso-Arabic)"
    , "Quechua"
    , "Rajasthani"
    , "Rajasthani(Perso-Arabic)"
    , "Rajasthani(Devanagari)"
    , "Rapanui"
    , "Rarotongan"
    , "Romance Language"
    , "Romanian"
    , "Romanian, Moldova"
    , "Romanian, Romania"
    , "Romansh"
    , "Romany"
    , "Rundi"
    , "Russian"
    , "Russian, Russia"
    , "Russian, Ukraine"
    , "Russian, Kazakhstan"
    , "Salishan Language"
    , "Samaritan Aramaic"
    , "Samaritan Aramaic(Syriac)"
    , "Sami Language"
    , "Samoan"
    , "Sandawe"
    , "Sango"
    , "Sanskrit"
    , "Sanskrit, India"
    , "Santali"
    , "Santali(Bengali)"
    , "Santali(Devanagari)"
    , "Santali(Latin)"
    , "Santali(Oriya)"
    , "Sardinian"
    , "Sasak"
    , "Scots"
    , "Selkup"
    , "Semitic Language"
    , "Serbian"
    , "Serbian, Bosnia and Herzegovina"
    , "Serbian, Bosnia and Herzegovina(Cyrillic)"
    , "Serbian, Bosnia and Herzegovina(Latin)"
    , "Serbian(Cyrillic)"
    , "Serbian(Latin)"
    , "Serbian, Montenegro"
    , "Serbian, Montenegro(Cyrillic)"
    , "Serbian, Montenegro(Latin)"
    , "Serbian, Serbia"
    , "Serbian, Serbia and Montenegro"
    , "Serbian, Serbia and Montenegro(Cyrillic)"
    , "Serbian, Serbia and Montenegro(Latin)"
    , "Serbian, Serbia(Cyrillic)"
    , "Serbian, Serbia(Latin)"
    , "Serbo-Croatian"
    , "Serbo-Croatian, Bosnia and Herzegovina"
    , "Serbo-Croatian, Montenegro"
    , "Serbo-Croatian, Serbia and Montenegro"
    , "Serer"
    , "Serer(Perso-Arabic)"
    , "Serer(Latin)"
    , "Shan"
    , "Shona"
    , "Sichuan Yi"
    , "Sichuan Yi, China"
    , "Sichuan Yi, China(Yi)"
    , "Sichuan Yi(Yi)"
    , "Sicilian"
    , "Sidamo"
    , "Sidamo, Ethiopia"
    , "Sidamo(Ethiopic)"
    , "Sidamo(Latin)"
    , "Sign Language"
    , "Siksika"
    , "Sindhi"
    , "Sindhi(Perso-Arabic)"
    , "Sindhi(Devanagari)"
    , "Sindhi(Gurmukhi)"
    , "Sinhala"
    , "Sinhala, Sri Lanka"
    , "Sino-Tibetan Language"
    , "Siouan Language"
    , "Skolt Sami"
    , "Slave"
    , "Slavic Language"
    , "Slovak"
    , "Slovak, Slovakia"
    , "Slovenian"
    , "Slovenian, Slovenia"
    , "Sogdien"
    , "Somali"
    , "Somali(Perso-Arabic)"
    , "Somali, Djibouti"
    , "Somali, Ethiopia"
    , "Somali, Kenya"
    , "Somali, Somalia"
    , "Songhai"
    , "Soninke"
    , "Soninke(Perso-Arabic)"
    , "Soninke(Latin)"
    , "Sorbian Language"
    , "Southern Sotho"
    , "Southern Sotho, Lesotho"
    , "Southern Sotho, South Africa"
    , "South American Indian Language"
    , "Southern Altai"
    , "Southern Sami"
    , "Spanish"
    , "Spanish, Argentina"
    , "Spanish, Bolivia"
    , "Spanish, Chile"
    , "Spanish, Colombia"
    , "Spanish, Costa Rica"
    , "Spanish, Dominican Republic"
    , "Spanish, Ecuador"
    , "Spanish, El Salvador"
    , "Spanish, Guatemala"
    , "Spanish, Honduras"
    , "Spanish, Mexico"
    , "Spanish, Nicaragua"
    , "Spanish, Panama"
    , "Spanish, Paraguay"
    , "Spanish, Peru"
    , "Spanish, Puerto Rico"
    , "Spanish, Spain"
    , "Spanish, United States"
    , "Spanish, Uruguay"
    , "Spanish, Venezuela"
    , "Sranan Tongo"
    , "Sukuma"
    , "Sumerian"
    , "Sundanese"
    , "Sundanese(Perso-Arabic)"
    , "Sundanese(Javanese)"
    , "Sundanese(Latin)"
    , "Susu"
    , "Susu(Perso-Arabic)"
    , "Susu(Latin)"
    , "Swahili"
    , "Swahili, Kenya"
    , "Swahili, Tanzania"
    , "Swati"
    , "Swati, South Africa"
    , "Swati, Swaziland"
    , "Swedish"
    , "Swedish, Finland"
    , "Swedish, Sweden"
    , "Swiss German"
    , "Swiss German, Switzerland"
    , "Syriac"
    , "Syriac(Cyrillic)"
    , "Syriac, Syria"
    , "Syriac(Syriac)"
    , "Syriac, Syria(Cyrillic)"
    , "Tagalog"
    , "Tahitian"
    , "Tai Language"
    , "Tajik"
    , "Tajik(Perso-Arabic)"
    , "Tajik(Cyrillic)"
    , "Tajik(Latin)"
    , "Tajik, Tajikistan"
    , "Tajik, Tajikistan(Perso-Arabic)"
    , "Tajik, Tajikistan(Cyrillic)"
    , "Tajik, Tajikistan(Latin)"
    , "Tamashek"
    , "Tamashek(Perso-Arabic)"
    , "Tamashek(Latin)"
    , "Tamashek(Tifinagh)"
    , "Tamil"
    , "Tamil, India"
    , "Tatar"
    , "Tatar(Cyrillic)"
    , "Tatar(Latin)"
    , "Tatar, Russia"
    , "Tatar, Russia(Cyrillic)"
    , "Tatar, Russia(Latin)"
    , "Telugu"
    , "Telugu, India"
    , "Tereno"
    , "Tetum"
    , "Thai"
    , "Thai, Thailand"
    , "Tibetan"
    , "Tibetan, China"
    , "Tibetan, India"
    , "Tigre"
    , "Tigre, Eritrea"
    , "Tigrinya"
    , "Tigrinya, Eritrea"
    , "Tigrinya, Ethiopia"
    , "Timne"
    , "Tiv"
    , "Tlingit"
    , "Tokelau"
    , "Tok Pisin"
    , "Nyasa Tonga"
    , "Nyasa Tonga, Tonga"
    , "Tonga"
    , "Tsimshian"
    , "Tsimshian, South Africa"
    , "Tsonga"
    , "Tswana"
    , "Tswana, South Africa"
    , "Tumbuka"
    , "Tupi Language"
    , "Turkish"
    , "Turkish, Turkey"
    , "Turkmen"
    , "Turkmen(Perso-Arabic)"
    , "Turkmen(Cyrillic)"
    , "Turkmen(Latin)"
    , "Tuvalu"
    , "Tuvinian"
    , "Twi"
    , "Tyap"
    , "Tyap, Nigeria"
    , "Udmurt"
    , "Udmurt(Cyrillic)"
    , "Udmurt(Latin)"
    , "Ugaritic"
    , "Uyghur"
    , "Uyghur(Perso-Arabic)"
    , "Uyghur, China"
    , "Uyghur, China(Perso-Arabic)"
    , "Uyghur, China(Cyrillic)"
    , "Uyghur, China(Latin)"
    , "Uyghur(Cyrillic)"
    , "Uyghur(Latin)"
    , "Ukrainian"
    , "Ukrainian, Ukraine"
    , "Umbundu"
    , "Miscellaneous Language"
    , "Unknown Language"
    , "Upper Sorbian"
    , "Urdu"
    , "Urdu(Perso-Arabic)"
    , "Urdu, India"
    , "Urdu, Pakistan"
    , "Uzbek"
    , "Uzbek, Afghanistan"
    , "Uzbek, Afghanistan(Perso-Arabic)"
    , "Uzbek(Perso-Arabic)"
    , "Uzbek(Cyrillic)"
    , "Uzbek(Latin)"
    , "Uzbek, Uzbekistan"
    , "Uzbek, Uzbekistan(Cyrillic)"
    , "Uzbek, Uzbekistan(Latin)"
    , "Vai"
    , "Venda"
    , "Venda, South Africa"
    , "Vietnamese"
    , "Vietnamese, Vietnam"
    , "Volapük"
    , "Votic"
    , "Wakashan Language"
    , "Walamo"
    , "Walamo, Ethiopia"
    , "Walloon"
    , "Waray"
    , "Washo"
    , "Welsh"
    , "Welsh, United Kingdom"
    , "Western Frisian"
    , "Wolof"
    , "Wolof(Perso-Arabic)"
    , "Wolof(Latin)"
    , "Wolof, Senegal"
    , "Wolof, Senegal(Perso-Arabic)"
    , "Wolof, Senegal(Latin)"
    , "Xhosa"
    , "Xhosa, South Africa"
    , "Yakut"
    , "Yao"
    , "Yapese"
    , "Yiddish"
    , "Yiddish(Hebrew)"
    , "Yoruba"
    , "Yoruba, Nigeria"
    , "Yupik Language"
    , "Zande"
    , "Zapotec"
    , "Zaza"
    , "Zenaga"
    , "Zhuang"
    , "Zulu"
    , "Zulu, South Africa"
    , "Zuni"
    ]
