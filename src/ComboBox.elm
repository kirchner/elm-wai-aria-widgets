module ComboBox
    exposing
        ( Behaviour
        , ComboBox
        , DisplayCondition
        , HtmlAttributes
        , HtmlDetails
        , Ids
        , Msg
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

@docs UpdateConfig, updateConfig, Behaviour

@docs DisplayCondition, matchingQuery, onFocus, onDemand

@docs ViewConfig, viewConfig, Views, HtmlAttributes, HtmlDetails

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
import Json.Decode as Decode exposing (Decoder)
import Listbox exposing (Listbox, TypeAhead)
import Task


{-| TODO
-}
type ComboBox
    = ComboBox Data


type alias Data =
    { preventBlur : Bool
    , open : Bool
    , query : String
    , listbox : Listbox
    }


{-| TODO
-}
closed : ComboBox
closed =
    ComboBox
        { preventBlur = False
        , open = False
        , query = ""
        , listbox = Listbox.unfocused
        }



---- VIEW CONFIG


{-| TODO
-}
type ViewConfig a
    = ViewConfig (a -> String) (String -> a -> Bool) (Views a)


{-| TODO
-}
viewConfig : (a -> String) -> (String -> a -> Bool) -> Views a -> ViewConfig a
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views a =
    { container : HtmlAttributes
    , placeholder : String
    , printEntry : a -> String
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
    }


{-| TODO
-}
type alias HtmlAttributes =
    List (Html.Attribute Never)


{-| TODO
-}
type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }



---- UPDATE CONFIG


{-| TODO
-}
type UpdateConfig a
    = UpdateConfig (a -> String) (String -> a -> Bool) Behaviour


{-| TODO
-}
updateConfig : (a -> String) -> (String -> a -> Bool) -> Behaviour -> UpdateConfig a
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
view : ViewConfig a -> Ids -> ComboBox -> List a -> Maybe a -> Html (Msg a)
view (ViewConfig uniqueId matchesQuery views) ids (ComboBox data) allEntries maybeSelection =
    let
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
                            , maybeQuery = Just data.query
                            }
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
            List.filter (matchesQuery data.query) allEntries
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
             , maybeSelection
                |> Maybe.map views.printEntry
                |> Maybe.withDefault views.placeholder
                |> Attributes.placeholder
             , Attributes.value data.query
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
    -> List a
    -> Maybe a
    -> Msg a
    -> ( ComboBox, Cmd (Msg a), Maybe outMsg )
update config entrySelected ((ComboBox data) as comboBox) allEntries maybeSelection msg =
    let
        (UpdateConfig uniqueId matchesQuery behaviour) =
            config
    in
    case msg of
        -- TEXTFIELD
        TextfieldFocused id ->
            ( case behaviour.displayCondition of
                MatchingQuery minimalLength ->
                    let
                        filteredEntries =
                            List.filter (matchesQuery data.query) allEntries
                    in
                    if
                        (String.length data.query >= minimalLength)
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
                    let
                        filteredEntries =
                            List.filter (matchesQuery newQuery) allEntries
                    in
                    if
                        (String.length newQuery >= minimalLength)
                            && (List.length filteredEntries >= 1)
                    then
                        ComboBox
                            { data
                                | open = True
                                , query = newQuery
                            }
                    else
                        ComboBox
                            { data
                                | open = False
                                , query = newQuery
                            }

                OnFocus ->
                    ComboBox { data | query = newQuery }

                OnDemand ->
                    ComboBox { data | query = newQuery }
            , Cmd.none
            , Nothing
            )

        TextfieldArrowUpPressed id maybeScrollData ->
            let
                filteredEntries =
                    List.filter (matchesQuery data.query) allEntries

                listboxConfig =
                    Listbox.updateConfig uniqueId
                        { jumpAtEnds = behaviour.jumpAtEnds
                        , separateFocus = behaviour.separateFocus
                        , selectionFollowsFocus = behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = behaviour.handleHomeAndEnd
                        , typeAhead = Listbox.noTypeAhead
                        }
            in
            if data.open then
                let
                    ( newListbox, listboxCmd ) =
                        Listbox.focusPreviousEntry
                            listboxConfig
                            (printListboxId id)
                            filteredEntries
                            []
                            maybeScrollData
                            data.listbox
                in
                ( ComboBox { data | listbox = newListbox }
                , Cmd.map (ListboxMsg (Just id)) listboxCmd
                , Nothing
                )
            else
                ( comboBox, Cmd.none, Nothing )

        TextfieldArrowDownPressed id maybeScrollData ->
            let
                filteredEntries =
                    List.filter (matchesQuery data.query) allEntries

                listboxConfig =
                    Listbox.updateConfig uniqueId
                        { jumpAtEnds = behaviour.jumpAtEnds
                        , separateFocus = behaviour.separateFocus
                        , selectionFollowsFocus = behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = behaviour.handleHomeAndEnd
                        , typeAhead = Listbox.noTypeAhead
                        }
            in
            if data.open then
                let
                    ( newListbox, listboxCmd ) =
                        Listbox.focusFirstOrNextEntry
                            listboxConfig
                            (printListboxId id)
                            filteredEntries
                            []
                            maybeScrollData
                            data.listbox
                in
                ( ComboBox { data | listbox = newListbox }
                , Cmd.map (ListboxMsg (Just id)) listboxCmd
                , Nothing
                )
            else
                ( comboBox, Cmd.none, Nothing )

        TextfieldEnterPressed ->
            let
                listboxConfig =
                    Listbox.updateConfig uniqueId
                        { jumpAtEnds = behaviour.jumpAtEnds
                        , separateFocus = behaviour.separateFocus
                        , selectionFollowsFocus = behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = behaviour.handleHomeAndEnd
                        , typeAhead = Listbox.noTypeAhead
                        }

                filteredEntries =
                    List.filter (matchesQuery data.query) allEntries
            in
            case Listbox.focusedEntry listboxConfig data.listbox filteredEntries of
                Nothing ->
                    ( comboBox, Cmd.none, Nothing )

                Just newEntry ->
                    ( ComboBox
                        { data
                            | open = False
                            , query = ""
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
                            , query = ""
                        }
                    , comboBoxCmd
                    , Just (entrySelected a)
                    )

        NoOp ->
            ( comboBox, Cmd.none, Nothing )


focusTextfield : String -> Cmd (Msg a)
focusTextfield id =
    Browser.focus (printTextfieldId id)
        |> Task.attempt (\_ -> NoOp)



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : ComboBox -> Sub (Msg a)
subscriptions (ComboBox { listbox, open }) =
    if open then
        Sub.map (ListboxMsg Nothing) (Listbox.subscriptions listbox)
    else
        Sub.none
