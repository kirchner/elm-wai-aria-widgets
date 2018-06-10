module Widget.ComboBox
    exposing
        ( Behaviour
        , ComboBox
        , DisplayCondition
        , Ids
        , Msg
        , Shared
        , UpdateConfig
        , ViewConfig
        , Views
        , closed
        , matchingQuery
        , onDemand
        , onFocus
        , subscriptions
        , update
        , updateConfig
        , view
        , viewConfig
        )

{-|

@docs ComboBox, closed, view, Ids, update, Msg, subscriptions


# Configuration

@docs Shared

@docs UpdateConfig, updateConfig, Behaviour

@docs DisplayCondition, matchingQuery, onFocus, onDemand

@docs ViewConfig, viewConfig, Views

-}

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
import Internal.Entries exposing (Entry(..))
import Json.Decode as Decode exposing (Decoder)
import Task
import Widget exposing (HtmlAttributes, HtmlDetails)
import Widget.Listbox as Listbox exposing (Listbox, TypeAhead)


{-| TODO
-}
type ComboBox
    = ComboBox Data


type alias Data =
    { preventBlur : Bool
    , open : Bool
    , query : Maybe String
    , listbox : Listbox
    }


{-| TODO
-}
closed : ComboBox
closed =
    ComboBox
        { preventBlur = False
        , open = False
        , query = Nothing
        , listbox = Listbox.unfocused
        }



---- SHARED CONFIG


{-| TODO
-}
type alias Shared a =
    { uniqueId : a -> String
    , matchesQuery : String -> a -> Bool
    , printEntry : a -> String
    }



---- VIEW CONFIG


{-| TODO
-}
type ViewConfig a divider
    = ViewConfig (Shared a) (Views a divider)


{-| TODO
-}
viewConfig : Shared a -> Views a divider -> ViewConfig a divider
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views a divider =
    { container : HtmlAttributes
    , placeholder : String
    , textfield :
        { maybeSelection : Maybe a
        , open : Bool
        }
        -> HtmlAttributes
    , ul : HtmlAttributes
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , liDivider : divider -> HtmlDetails
    }



---- UPDATE CONFIG


{-| TODO
-}
type UpdateConfig a
    = UpdateConfig (Shared a) Behaviour


{-| TODO
-}
updateConfig : Shared a -> Behaviour -> UpdateConfig a
updateConfig =
    UpdateConfig


{-| TODO
-}
type alias Behaviour =
    { jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , displayCondition : DisplayCondition
    }


{-| TODO
-}
type DisplayCondition
    = MatchingQuery Int
    | OnFocus
    | OnDemand


{-| TODO
-}
matchingQuery : Int -> DisplayCondition
matchingQuery =
    MatchingQuery


{-| TODO
-}
onFocus : DisplayCondition
onFocus =
    OnFocus


{-| TODO
-}
onDemand : DisplayCondition
onDemand =
    OnDemand



---- VIEW


{-| TODO
-}
type alias Ids =
    { id : String
    , labelledBy : String
    }


{-| TODO
-}
view :
    ViewConfig a divider
    -> Ids
    -> ComboBox
    -> List (Entry a divider)
    -> Maybe a
    -> Html (Msg a)
view config ids (ComboBox data) allEntries maybeSelection =
    let
        (ViewConfig { uniqueId, matchesQuery, printEntry } views) =
            config

        textfieldHtmlAttributes open =
            views.textfield
                { maybeSelection = maybeSelection
                , open = open
                }

        containerHtmlAttributes =
            views.container

        listboxConfig =
            Listbox.viewConfig uniqueId
                { ul = Attributes.style "position" "absolute" :: views.ul
                , li =
                    \{ selected, keyboardFocused, mouseFocused } ->
                        views.li
                            { selected = selected
                            , keyboardFocused = keyboardFocused
                            , mouseFocused = mouseFocused
                            , maybeQuery = data.query
                            }
                , liDivider = views.liDivider
                , empty = Html.text ""
                , focusable = False
                }

        selection =
            case maybeSelection of
                Nothing ->
                    []

                Just actualSelection ->
                    [ actualSelection ]

        filteredEntries =
            filterEntries (matchesQuery query) allEntries

        query =
            data.query
                |> or (Maybe.map printEntry maybeSelection)
                |> Maybe.withDefault ""
    in
    Html.div
        (appendAttributes containerHtmlAttributes [])
        [ Html.input
            ([ Attributes.id (printTextfieldId ids.id)
             , Attributes.type_ "text"
             , Attributes.attribute "aria-haspopup" "listbox"
             , Attributes.attribute "aria-labelledby"
                (printTextfieldId ids.id ++ " " ++ ids.labelledBy)
             , Attributes.style "position" "relative"
             , Attributes.tabindex 0
             , Attributes.placeholder views.placeholder
             , Attributes.value query
             , Attributes.autocomplete False
             , Events.onFocus (TextfieldFocused ids.id)
             , Events.onBlur (TextfieldBlured ids.id)
             , Events.onInput TextfieldChanged
             , Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "ArrowUp" ->
                                    filteredEntries
                                        |> Listbox.arrowUpDecoder listboxConfig
                                            [ "target", "nextSibling" ]
                                            data.listbox
                                        |> Decode.map (TextfieldArrowUpPressed ids.id)

                                "ArrowDown" ->
                                    filteredEntries
                                        |> Listbox.arrowDownDecoder listboxConfig
                                            [ "target", "nextSibling" ]
                                            data.listbox
                                        |> Decode.map (TextfieldArrowDownPressed ids.id)

                                "Enter" ->
                                    Decode.succeed TextfieldEnterPressed

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                    |> preventDefault
                )
             ]
                |> setAriaExpanded data.open
                |> appendAttributes (textfieldHtmlAttributes data.open)
            )
            []
        , if data.open then
            case filteredEntries of
                [] ->
                    Html.text ""

                _ ->
                    Listbox.view listboxConfig
                        { id = printListboxId ids.id
                        , labelledBy = ids.labelledBy
                        }
                        data.listbox
                        filteredEntries
                        selection
                        |> Html.map (ListboxMsg (Just ids.id))
          else
            Html.text ""
        ]



-- VIEW HELPER


appendAttributes :
    List (Html.Attribute Never)
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
appendAttributes neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> NoOp))
        |> List.append attrs


preventDefault : Decoder msg -> Decoder ( msg, Bool )
preventDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, True ))


setAriaExpanded : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setAriaExpanded isOpen attrs =
    if isOpen then
        Attributes.attribute "aria-expanded" "true" :: attrs
    else
        attrs



-- IDS


printTextfieldId : String -> String
printTextfieldId id =
    id ++ "__textfield"


printListboxId : String -> String
printListboxId id =
    id ++ "-listbox"



---- UPDATE


{-| TODO
-}
type Msg a
    = NoOp
      -- TEXTFIELD
    | TextfieldFocused String
    | TextfieldBlured String
    | TextfieldChanged String
    | TextfieldArrowUpPressed String (Maybe Listbox.ScrollData)
    | TextfieldArrowDownPressed String (Maybe Listbox.ScrollData)
    | TextfieldEnterPressed
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)


type OutMsg a
    = ListMouseDown
    | ListMouseUp
    | EntrySelected a


{-| TODO
-}
update :
    UpdateConfig a
    -> (a -> outMsg)
    -> ComboBox
    -> List (Entry a divider)
    -> Maybe a
    -> Msg a
    -> ( ComboBox, Cmd (Msg a), Maybe outMsg )
update config entrySelected ((ComboBox data) as comboBox) allEntries maybeSelection msg =
    let
        (UpdateConfig { uniqueId, matchesQuery, printEntry } behaviour) =
            config

        query =
            data.query
                |> or (Maybe.map printEntry maybeSelection)
                |> Maybe.withDefault ""
    in
    case msg of
        -- TEXTFIELD
        TextfieldFocused id ->
            ( case behaviour.displayCondition of
                MatchingQuery minimalLength ->
                    let
                        filteredEntries =
                            List.filter matches allEntries

                        matches entry =
                            case entry of
                                Divider _ ->
                                    False

                                Entry a ->
                                    matchesQuery query a
                    in
                    if
                        (String.length query >= minimalLength)
                            && (List.length filteredEntries > 1)
                    then
                        ComboBox
                            { data
                                | open = True
                                , preventBlur = False
                            }
                    else
                        comboBox

                OnFocus ->
                    ComboBox
                        { data
                            | open = True
                            , preventBlur = False
                        }

                OnDemand ->
                    comboBox
            , Cmd.none
            , Nothing
            )

        TextfieldBlured id ->
            ( if data.preventBlur then
                comboBox
              else
                ComboBox { data | open = False }
            , if data.preventBlur then
                focusTextfield id
              else
                Cmd.none
            , Nothing
            )

        TextfieldChanged newQuery ->
            ( case behaviour.displayCondition of
                MatchingQuery minimalLength ->
                    if
                        (String.length newQuery >= minimalLength)
                            && (List.length (filterEntries (matchesQuery query) allEntries) >= 1)
                    then
                        ComboBox
                            { data
                                | open = True
                                , query = Just newQuery
                            }
                    else
                        ComboBox
                            { data
                                | open = False
                                , query = Just newQuery
                            }

                OnFocus ->
                    ComboBox { data | query = Just newQuery }

                OnDemand ->
                    ComboBox { data | query = Just newQuery }
            , Cmd.none
            , Nothing
            )

        TextfieldArrowUpPressed id maybeScrollData ->
            if data.open then
                let
                    filteredEntries =
                        filterEntries (matchesQuery query) allEntries

                    listboxConfig =
                        Listbox.updateConfig uniqueId
                            { jumpAtEnds = behaviour.jumpAtEnds
                            , separateFocus = behaviour.separateFocus
                            , selectionFollowsFocus = behaviour.selectionFollowsFocus
                            , handleHomeAndEnd = behaviour.handleHomeAndEnd
                            , typeAhead = Listbox.noTypeAhead
                            }

                    ( newListbox, maybeOutMsg ) =
                        Listbox.focusPreviousOrFirstEntry listboxConfig filteredEntries data.listbox
                in
                ( ComboBox { data | listbox = newListbox }
                , Cmd.map (ListboxMsg (Just id)) <|
                    Listbox.scrollToFocus (printListboxId id) newListbox maybeScrollData
                , Nothing
                )
            else
                ( comboBox, Cmd.none, Nothing )

        TextfieldArrowDownPressed id maybeScrollData ->
            if data.open then
                let
                    filteredEntries =
                        filterEntries (matchesQuery query) allEntries

                    listboxConfig =
                        Listbox.updateConfig uniqueId
                            { jumpAtEnds = behaviour.jumpAtEnds
                            , separateFocus = behaviour.separateFocus
                            , selectionFollowsFocus = behaviour.selectionFollowsFocus
                            , handleHomeAndEnd = behaviour.handleHomeAndEnd
                            , typeAhead = Listbox.noTypeAhead
                            }

                    ( newListbox, maybeOutMsg ) =
                        Listbox.focusNextOrFirstEntry listboxConfig filteredEntries data.listbox
                in
                ( ComboBox { data | listbox = newListbox }
                , Cmd.map (ListboxMsg (Just id)) <|
                    Listbox.scrollToFocus (printListboxId id) newListbox maybeScrollData
                , Nothing
                )
            else
                ( comboBox, Cmd.none, Nothing )

        TextfieldEnterPressed ->
            let
                filteredEntries =
                    filterEntries (matchesQuery query) allEntries

                listboxConfig =
                    Listbox.updateConfig uniqueId
                        { jumpAtEnds = behaviour.jumpAtEnds
                        , separateFocus = behaviour.separateFocus
                        , selectionFollowsFocus = behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = behaviour.handleHomeAndEnd
                        , typeAhead = Listbox.noTypeAhead
                        }
            in
            case Listbox.focusedEntry listboxConfig data.listbox filteredEntries of
                Nothing ->
                    ( comboBox, Cmd.none, Nothing )

                Just newEntry ->
                    ( ComboBox
                        { data
                            | open = False
                            , query = Nothing
                        }
                    , Cmd.none
                    , Just (entrySelected newEntry)
                    )

        -- LISTBOX
        ListboxMsg maybeId listboxMsg ->
            let
                selection =
                    case maybeSelection of
                        Nothing ->
                            []

                        Just actualSelection ->
                            [ actualSelection ]

                listboxConfig =
                    Listbox.updateConfig uniqueId
                        { jumpAtEnds = behaviour.jumpAtEnds
                        , separateFocus = behaviour.separateFocus
                        , selectionFollowsFocus = behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = behaviour.handleHomeAndEnd
                        , typeAhead = Listbox.noTypeAhead
                        }

                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update listboxConfig
                        [ Listbox.onMouseDown ListMouseDown
                        , Listbox.onMouseUp ListMouseUp
                        , Listbox.onEntrySelect EntrySelected
                        ]
                        data.listbox
                        allEntries
                        selection
                        listboxMsg

                newData =
                    { data | listbox = newListbox }

                comboBoxCmd =
                    Cmd.map (ListboxMsg maybeId) listboxCmd
            in
            case maybeOutMsg of
                Nothing ->
                    ( ComboBox newData
                    , comboBoxCmd
                    , Nothing
                    )

                Just ListMouseDown ->
                    ( ComboBox { newData | preventBlur = True }
                    , Cmd.batch
                        [ comboBoxCmd
                        , maybeId
                            |> Maybe.map focusTextfield
                            |> Maybe.withDefault Cmd.none
                        ]
                    , Nothing
                    )

                Just ListMouseUp ->
                    ( ComboBox { newData | preventBlur = False }
                    , Cmd.batch
                        [ comboBoxCmd
                        , maybeId
                            |> Maybe.map focusTextfield
                            |> Maybe.withDefault Cmd.none
                        ]
                    , Nothing
                    )

                Just (EntrySelected a) ->
                    ( ComboBox
                        { data
                            | open = False
                            , query = Nothing
                        }
                    , comboBoxCmd
                    , Just (entrySelected a)
                    )

        NoOp ->
            ( comboBox, Cmd.none, Nothing )


filterEntries : (a -> Bool) -> List (Entry a divider) -> List (Entry a divider)
filterEntries matchesQuery entries =
    let
        matches entry =
            case entry of
                Divider _ ->
                    True

                Entry a ->
                    matchesQuery a
    in
    List.filter matches entries


focusTextfield : String -> Cmd (Msg a)
focusTextfield id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)


or : Maybe a -> Maybe a -> Maybe a
or fallback default =
    case default of
        Nothing ->
            fallback

        Just _ ->
            default



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : ComboBox -> Sub (Msg a)
subscriptions (ComboBox { listbox, open }) =
    if open then
        Sub.map (ListboxMsg Nothing) (Listbox.subscriptions listbox)
    else
        Sub.none
