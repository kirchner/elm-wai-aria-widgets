module Widget.ComboBox exposing
    ( ComboBox, init, view, update, Msg, subscriptions
    , Shared
    , UpdateConfig, updateConfig, Behaviour
    , DisplayCondition, matchingQuery, onFocus, onDemand
    , ViewConfig, viewConfig, Views
    )

{-|

@docs ComboBox, init, view, update, Msg, subscriptions


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

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Listbox exposing (Entry(..))
import Json.Decode as Decode exposing (Decoder)
import Task
import Widget exposing (HtmlAttributes, HtmlDetails)
import Widget.Listbox as Listbox exposing (Listbox, TypeAhead)
import Widget.Listbox.Unique as ListboxUnique


{-| TODO
-}
type ComboBox
    = ComboBox Data


type alias Data =
    { preventBlur : Bool
    , open : Bool
    , listbox : Listbox
    }


{-| TODO
-}
init : ComboBox
init =
    ComboBox
        { preventBlur = False
        , open = False
        , listbox = Listbox.init
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
        { query : String
        , open : Bool
        }
        -> HtmlAttributes
    , ul : HtmlAttributes
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , query : String
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
    , minimalGap : Float
    , initialGap : Float
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
view :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        }
    -> List (Entry a divider)
    -> ComboBox
    -> String
    -> Html (Msg a)
view config ids allEntries (ComboBox data) query =
    let
        (ViewConfig { uniqueId, matchesQuery, printEntry } views) =
            config

        textfieldHtmlAttributes open =
            views.textfield
                { query = query
                , open = open
                }

        containerHtmlAttributes =
            views.container

        listboxConfig =
            Listbox.viewConfig uniqueId
                { ul = Attributes.style "position" "absolute" :: views.ul
                , liOption =
                    \{ selected, focused, hovered } ->
                        views.liOption
                            { selected = selected
                            , focused = focused
                            , hovered = hovered
                            , query = query
                            }
                , liDivider = views.liDivider
                , empty = Html.text ""
                , focusable = False
                }

        filteredEntries =
            filterEntries (matchesQuery query) allEntries
    in
    Html.div
        (appendAttributes containerHtmlAttributes
            [ Events.onMouseDown (ListboxMouseDown ids.id)
            , Events.onMouseUp (ListboxMouseUp ids.id)
            , Events.on "click"
                (Decode.at [ "target", "id" ] Decode.string
                    |> Decode.andThen
                        (\targetId ->
                            if targetId == printTextfieldId ids.id then
                                Decode.fail "not handling click event here"

                            else
                                Decode.succeed (ListboxClicked ids.id)
                        )
                )
            ]
        )
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
             , Listbox.preventDefaultOnKeyDown
                { id = printListboxId ids.id
                , labelledBy = ids.labelledBy
                , lift = ListboxMsg (Just ids.id)
                }
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "Enter" ->
                                    Decode.succeed ( TextfieldEnterPressed, True )

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
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
                    ListboxUnique.view listboxConfig
                        { id = printListboxId ids.id
                        , labelledBy = ids.labelledBy
                        , lift = ListboxMsg (Just ids.id)
                        }
                        filteredEntries
                        data.listbox
                        Nothing

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
    | TextfieldEnterPressed
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)
    | ListboxMouseDown String
    | ListboxMouseUp String
    | ListboxClicked String


{-| TODO
-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> ComboBox
    -> String
    -> ( ComboBox, Cmd (Msg a), String )
update config allEntries msg ((ComboBox data) as comboBox) query =
    let
        (UpdateConfig { uniqueId, matchesQuery, printEntry } behaviour) =
            config
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

                                Option a ->
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
            , query
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
            , query
            )

        TextfieldChanged newQuery ->
            ( case behaviour.displayCondition of
                MatchingQuery minimalLength ->
                    let
                        filteredEntries =
                            List.filter matches allEntries

                        matches entry =
                            case entry of
                                Divider _ ->
                                    False

                                Option a ->
                                    matchesQuery newQuery a
                    in
                    if
                        (String.length newQuery >= minimalLength)
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
            , newQuery
            )

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
                        , minimalGap = behaviour.minimalGap
                        , initialGap = behaviour.initialGap
                        }
            in
            case Listbox.focusedEntry listboxConfig data.listbox filteredEntries of
                Nothing ->
                    ( comboBox, Cmd.none, query )

                Just newEntry ->
                    ( ComboBox
                        { data
                            | open = False
                            , listbox = Listbox.init
                        }
                    , Cmd.none
                    , printEntry newEntry
                    )

        -- LISTBOX
        ListboxMsg maybeId listboxMsg ->
            let
                listboxConfig =
                    Listbox.updateConfig uniqueId
                        { jumpAtEnds = behaviour.jumpAtEnds
                        , separateFocus = behaviour.separateFocus
                        , selectionFollowsFocus = behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = behaviour.handleHomeAndEnd
                        , typeAhead = Listbox.noTypeAhead
                        , minimalGap = behaviour.minimalGap
                        , initialGap = behaviour.initialGap
                        }

                ( newListbox, listboxCmd, newSelection ) =
                    ListboxUnique.update listboxConfig
                        filteredEntries
                        listboxMsg
                        data.listbox
                        Nothing

                filteredEntries =
                    List.filter matches allEntries

                matches entry =
                    case entry of
                        Divider _ ->
                            False

                        Option a ->
                            matchesQuery query a

                newData =
                    { data | listbox = newListbox }

                comboBoxCmd =
                    Cmd.map (ListboxMsg maybeId) listboxCmd
            in
            ( ComboBox newData
            , comboBoxCmd
            , case newSelection of
                Nothing ->
                    query

                Just a ->
                    printEntry a
            )

        ListboxMouseDown id ->
            ( ComboBox { data | preventBlur = True }
            , focusTextfield id
            , query
            )

        ListboxMouseUp id ->
            ( ComboBox { data | preventBlur = False }
            , focusTextfield id
            , query
            )

        ListboxClicked id ->
            ( ComboBox { data | open = False }
            , focusTextfield id
            , query
            )

        NoOp ->
            ( comboBox, Cmd.none, query )


filterEntries : (a -> Bool) -> List (Entry a divider) -> List (Entry a divider)
filterEntries matchesQuery entries =
    let
        matches entry =
            case entry of
                Divider _ ->
                    True

                Option a ->
                    matchesQuery a
    in
    List.filter matches entries


focusTextfield : String -> Cmd (Msg a)
focusTextfield id =
    Dom.focus (printTextfieldId id)
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
