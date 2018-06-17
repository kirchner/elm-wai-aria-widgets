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
import Data exposing (allLocales, locales)
import DropdownMenus.ComboBox
import DropdownMenus.Listbox
import Helper exposing (matchesQuery)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Html
import Json.Decode as Decode
import Listboxes.MultiSelect as MultiSelect
import Listboxes.SingleSelect as SingleSelect
import Set exposing (Set)
import Task
import Views exposing (liChildren, viewCheckbox)
import Widget exposing (HtmlDetails)
import Widget.Accordion as Accordion exposing (Accordion, PanelState(..))
import Widget.Dialog as Dialog exposing (Dialog)
import Widget.Listbox as Listbox exposing (Entry, Listbox)
import Widget.Tabs as Tabs exposing (Tabs)


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
    { tabs : Tabs
    , tabsJumpAtEnds : Bool
    , tabsHandleHomeAndEnd : Bool
    , tabsActivateOnFocus : Bool

    -- ACCORDION
    , accordion : Accordion
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

    -- DROPDOWN MENUS
    , dropdownListbox : DropdownMenus.Listbox.Model
    , dropdownComboBox : DropdownMenus.ComboBox.Model

    -- LISTBOXES
    , rearrangableSingleSelect : SingleSelect.Model
    , multiSelect : MultiSelect.Model

    -- DIALOG
    , dialog : Dialog
    }


init _ =
    ( { tabs = Tabs.init "dialog"
      , tabsJumpAtEnds = True
      , tabsHandleHomeAndEnd = True
      , tabsActivateOnFocus = True
      , accordion = Accordion.init
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
      , dropdownListbox = DropdownMenus.Listbox.init
      , dropdownComboBox = DropdownMenus.ComboBox.init
      , rearrangableSingleSelect = SingleSelect.init
      , multiSelect = MultiSelect.init
      , dialog = Dialog.init
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = NoOp
    | TabsMsg (Cmd Msg) Tabs
    | TabsJumpAtEndsChecked Bool
    | TabsHandleHomeAndEndChecked Bool
    | TabsActivateOnFocusChecked Bool
      -- ACCORDION
    | AccordionMsg (Cmd Msg) Accordion
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
      -- DROPDOWN MENUS
    | DropdownListboxMsg DropdownMenus.Listbox.Msg
    | DropdownComboBoxMsg DropdownMenus.ComboBox.Msg
      -- LISTBOXES
    | RearrangableSingleSelectMsg SingleSelect.Msg
    | MultiSelectMsg MultiSelect.Msg
      -- DIALOG
    | OpenDialogClicked
    | DialogBackdropClicked
    | DialogCloseClicked
    | DialogCancelClicked
    | DialogSubmitClicked
    | DialogMsg (Cmd Msg)


type OutMsg
    = EntrySelected String
    | EntriesSelected (List String)
    | EntryUnselected String
    | AllEntriesSelected
    | AllEntriesUnselected


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TabsMsg cmd newTabs ->
            ( { model | tabs = newTabs }
            , cmd
            )

        TabsJumpAtEndsChecked enabled ->
            ( { model | tabsJumpAtEnds = enabled }
            , Cmd.none
            )

        TabsHandleHomeAndEndChecked enabled ->
            ( { model | tabsHandleHomeAndEnd = enabled }
            , Cmd.none
            )

        TabsActivateOnFocusChecked enabled ->
            ( { model | tabsActivateOnFocus = enabled }
            , Cmd.none
            )

        -- ACCORDION
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

        -- DROPDOWN MENUS
        DropdownListboxMsg subMsg ->
            let
                ( newState, subCmd ) =
                    DropdownMenus.Listbox.update subMsg model.dropdownListbox
            in
            ( { model | dropdownListbox = newState }
            , Cmd.map DropdownListboxMsg subCmd
            )

        DropdownComboBoxMsg subMsg ->
            let
                ( newState, subCmd ) =
                    DropdownMenus.ComboBox.update subMsg model.dropdownComboBox
            in
            ( { model | dropdownComboBox = newState }
            , Cmd.map DropdownComboBoxMsg subCmd
            )

        -- SINGLE-SELECT LISTBOX
        RearrangableSingleSelectMsg subMsg ->
            let
                ( newState, subCmd ) =
                    SingleSelect.update subMsg model.rearrangableSingleSelect
            in
            ( { model | rearrangableSingleSelect = newState }
            , Cmd.map RearrangableSingleSelectMsg subCmd
            )

        MultiSelectMsg subMsg ->
            let
                ( newState, subCmd ) =
                    MultiSelect.update subMsg model.multiSelect
            in
            ( { model | multiSelect = newState }
            , Cmd.map MultiSelectMsg subCmd
            )

        -- DIALOG
        OpenDialogClicked ->
            let
                ( newDialog, cmd ) =
                    Dialog.open DialogMsg "modal--street"
            in
            ( { model | dialog = newDialog }
            , cmd
            )

        DialogBackdropClicked ->
            ( { model | dialog = Dialog.init }
            , Cmd.none
            )

        DialogCloseClicked ->
            ( { model | dialog = Dialog.init }
            , Cmd.none
            )

        DialogCancelClicked ->
            ( { model | dialog = Dialog.init }
            , Cmd.none
            )

        DialogSubmitClicked ->
            ( { model | dialog = Dialog.init }
            , Cmd.none
            )

        DialogMsg cmd ->
            ( model
            , cmd
            )



---- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ Sub.map ListboxMsg (Listbox.subscriptions model.listbox)
        , Sub.map MuppetsListboxMsg (Listbox.subscriptions model.muppetsListbox)

        -- DROPDOWN MENUS
        , Sub.map DropdownListboxMsg (DropdownMenus.Listbox.subscriptions model.dropdownListbox)
        , Sub.map DropdownComboBoxMsg (DropdownMenus.ComboBox.subscriptions model.dropdownComboBox)

        -- LISTBOXES
        , Sub.map RearrangableSingleSelectMsg
            (SingleSelect.subscriptions model.rearrangableSingleSelect)
        , Sub.map MultiSelectMsg (MultiSelect.subscriptions model.multiSelect)
        ]



---- VIEW


view model =
    Html.div
        [ Attributes.class "section" ]
        [ Html.div [ Attributes.class "container" ]
            [ Tabs.view
                (tabConfig
                    model.tabsJumpAtEnds
                    model.tabsHandleHomeAndEnd
                    model.tabsActivateOnFocus
                )
                { id = "tabs"
                , label = "Widgets"
                , lift = TabsMsg
                }
                model.tabs
                [ Tabs.tab
                    { id = "basic"
                    , tab = "Basic"
                    , tabpanel = [ viewBasic model ]
                    , focusable = False
                    }
                , Tabs.tab
                    { id = "dropdown-menus"
                    , tab = "Dropdown Menus"
                    , tabpanel = [ viewDropdownMenus model ]
                    , focusable = False
                    }
                , Tabs.tab
                    { id = "listboxes"
                    , tab = "Listboxes"
                    , tabpanel = [ viewListboxes model ]
                    , focusable = False
                    }
                , Tabs.tab
                    { id = "dialog"
                    , tab = "Dialog"
                    , tabpanel = [ viewDialog model ]
                    , focusable = False
                    }
                , Tabs.tab
                    { id = "configuration"
                    , tab = "Configuration"
                    , tabpanel = [ viewConfiguration model ]
                    , focusable = False
                    }
                ]
            ]
        , Html.div
            [ Attributes.id "dialogs" ]
            [ Dialog.view dialogConfig DialogBackdropClicked model.dialog <|
                [ Html.div
                    [ Attributes.class "modal-card" ]
                    [ Html.header
                        [ Attributes.class "modal-card-head" ]
                        [ Html.p
                            [ Attributes.class "modal-card-title" ]
                            [ Html.text "Enter your address" ]
                        , Html.button
                            [ Attributes.class "delete"
                            , Events.onClick DialogCloseClicked
                            ]
                            []
                        ]
                    , Html.section
                        [ Attributes.class "modal-card-body" ]
                        [ Html.form
                            []
                            [ Html.div
                                [ Attributes.class "field" ]
                                [ Html.label
                                    [ Attributes.class "label" ]
                                    [ Html.text "Street" ]
                                , Html.div
                                    [ Attributes.class "control" ]
                                    [ Html.input
                                        [ Attributes.class "input"
                                        , Attributes.type_ "text"
                                        , Attributes.placeholder "Your street.."
                                        , Attributes.id "modal--street"
                                        , Dialog.wrapToEnd DialogMsg "modal--cancel"
                                        ]
                                        []
                                    ]
                                ]
                            , Html.div
                                [ Attributes.class "field" ]
                                [ Html.label
                                    [ Attributes.class "label" ]
                                    [ Html.text "City" ]
                                , Html.div
                                    [ Attributes.class "control" ]
                                    [ Html.input
                                        [ Attributes.class "input"
                                        , Attributes.type_ "text"
                                        , Attributes.placeholder "Your city.."
                                        ]
                                        []
                                    ]
                                ]
                            , Html.div
                                [ Attributes.class "field" ]
                                [ Html.label
                                    [ Attributes.class "label" ]
                                    [ Html.text "State" ]
                                , Html.div
                                    [ Attributes.class "control" ]
                                    [ Html.input
                                        [ Attributes.class "input"
                                        , Attributes.type_ "text"
                                        , Attributes.placeholder "Your state.."
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    , Html.footer
                        [ Attributes.class "modal-card-foot" ]
                        [ Html.button
                            [ Attributes.class "button"
                            , Attributes.class "is-success"
                            , Events.onClick DialogSubmitClicked
                            ]
                            [ Html.text "Submit address" ]
                        , Html.button
                            [ Attributes.class "button"
                            , Attributes.id "modal--cancel"
                            , Events.onClick DialogCancelClicked
                            , Dialog.wrapToStart DialogMsg "modal--street"
                            ]
                            [ Html.text "Cancel" ]
                        ]
                    ]
                ]
            ]
        ]


viewBasic model =
    Html.div
        [ Attributes.class "container"
        , Attributes.style "padding" "15px"
        ]
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
            ]
        ]


viewDropdownMenus : Model -> Html Msg
viewDropdownMenus model =
    Html.div
        [ Attributes.class "container"
        , Attributes.style "padding" "15px"
        ]
        [ Html.form
            [ Attributes.style "width" "100%" ]
            [ Html.div
                [ Attributes.class "columns" ]
                [ Html.div
                    [ Attributes.class "column" ]
                    [ DropdownMenus.Listbox.view model.dropdownListbox
                        |> Html.map DropdownListboxMsg
                    ]
                , Html.div
                    [ Attributes.class "column" ]
                    [ DropdownMenus.ComboBox.view model.dropdownComboBox
                        |> Html.map DropdownComboBoxMsg
                    ]
                ]
            ]
        ]


viewListboxes : Model -> Html Msg
viewListboxes model =
    Html.div
        [ Attributes.class "container"
        , Attributes.style "padding" "15px"
        ]
        [ Html.p
            [ Attributes.style "margin-bottom" "20px" ]
            [ Html.text "These are reimplementations of the "
            , Html.a
                [ Attributes.href "https://www.w3.org/TR/2017/NOTE-wai-aria-practices-1.1-20171214/examples/listbox/listbox-rearrangeable.html" ]
                [ Html.text "Example Listboxes with Rearrangable Optios" ]
            , Html.text " of the WAI-ARIA Authoring Practices 1.1."
            ]
        , Html.div
            [ Attributes.class "content"
            , Attributes.style "margin-top" "35px"
            ]
            [ Html.h4
                [ Attributes.class "title"
                , Attributes.class "is-4"
                ]
                [ Html.text "Example 1: Single-Select Listbox" ]
            , Html.p []
                [ Html.text "Rank features important to you when choosing where to live. If a feature is unimportant, move it to the unimportant features list."
                ]
            ]
        , SingleSelect.view model.rearrangableSingleSelect
            |> Html.map RearrangableSingleSelectMsg
        , Html.div
            [ Attributes.class "content"
            , Attributes.style "margin-top" "35px"
            ]
            [ Html.h4
                [ Attributes.class "title"
                , Attributes.class "is-4"
                ]
                [ Html.text "Example 2: Multi-Select Listbox" ]
            , Html.p []
                [ Html.text "Choose upgrades for your transport capsule." ]
            ]
        , MultiSelect.view model.multiSelect
            |> Html.map MultiSelectMsg
        ]


viewDialog : Model -> Html Msg
viewDialog model =
    Html.div
        [ Attributes.class "container"
        , Attributes.style "padding" "15px"
        ]
        [ Html.button
            [ Attributes.class "button"
            , Attributes.style "width" "auto"
            , Events.onClick OpenDialogClicked
            ]
            [ Html.text "Open Dialog" ]
        ]


viewConfiguration model =
    Html.form []
        [ Html.label
            [ Attributes.class "label" ]
            [ Html.text "Tabs" ]
        , viewCheckbox TabsJumpAtEndsChecked
            model.tabsJumpAtEnds
            "Jump at ends"
        , viewCheckbox TabsHandleHomeAndEndChecked
            model.tabsHandleHomeAndEnd
            "Handle Home and End keys"
        , viewCheckbox TabsActivateOnFocusChecked
            model.tabsActivateOnFocus
            "Activate tab when focused"
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
                    , lift = ListboxMsg
                    , onKeyDown = Decode.fail "not handling keys here"
                    }
                    listbox
                    locales
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
                    , lift = MuppetsListboxMsg
                    , onKeyDown = Decode.fail "not handling keys here"
                    }
                    listbox
                    muppets
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


dialogConfig =
    Dialog.viewConfig
        { container =
            \open ->
                [ Attributes.class "modal"
                , Attributes.classList
                    [ ( "is-active", open ) ]
                ]
        , backdrop = [ Attributes.class "modal-background" ]
        }



---- CONFIG


tabConfig : Bool -> Bool -> Bool -> Tabs.ViewConfig String
tabConfig jumpAtEnds handleHomeAndEnd activateOnFocus =
    Tabs.viewConfig
        { jumpAtEnds = jumpAtEnds
        , handleHomeAndEnd = handleHomeAndEnd
        , activateOnFocus = activateOnFocus
        }
        { tabs = [ Attributes.class "tabs-container" ]
        , tablist = [ Attributes.class "tablist" ]
        , button =
            \open title ->
                { attributes =
                    [ Attributes.class "tab-button"
                    , Attributes.classList
                        [ ( "tab-button--is-active", open ) ]
                    ]
                , children = [ Html.text title ]
                }
        , tabpanel = [ Attributes.class "tabpanel" ]
        }


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
                    [ Html.span
                        [ Attributes.class "accordion-title" ]
                        [ Html.text label ]
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
