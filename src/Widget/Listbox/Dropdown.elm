module Widget.Listbox.Dropdown
    exposing
        ( Behaviour
        , Dropdown
        , Msg
        , UpdateConfig
        , ViewConfig
        , Views
        , init
        , subscriptions
        , update
        , updateConfig
        , view
        , viewConfig
        )

{-|

@docs Dropdown, init, view, update, Msg, subscriptions


# Configuration

@docs UpdateConfig, updateConfig, Behaviour

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
import Internal.Entries
    exposing
        ( Next(..)
        , Previous(..)
        , findNext
        , findPrevious
        )
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Json.Decode as Decode exposing (Decoder)
import Task
import Widget exposing (HtmlAttributes, HtmlDetails)
import Widget.Listbox as Listbox exposing (Entry, Listbox, TypeAhead)


{-| TODO
-}
type Dropdown
    = Dropdown Data


type alias Data =
    { open : Bool
    , preventBlur : Bool
    , listbox : Listbox
    , pendingFocusListbox : Maybe String
    }


{-| TODO
-}
init : Dropdown
init =
    Dropdown
        { open = False
        , preventBlur = False
        , listbox = Listbox.init
        , pendingFocusListbox = Nothing
        }



---- VIEW CONFIG


{-| TODO
-}
type ViewConfig a divider
    = ViewConfig (a -> String) (Views a divider)


{-| TODO
-}
viewConfig : (a -> String) -> Views a divider -> ViewConfig a divider
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views a divider =
    { container : HtmlAttributes
    , button :
        { maybeSelection : Maybe a
        , open : Bool
        }
        -> HtmlDetails
    , ul : HtmlAttributes
    , liOption :
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
    = UpdateConfig (a -> String) (Behaviour a)


{-| TODO
-}
updateConfig : (a -> String) -> Behaviour a -> UpdateConfig a
updateConfig =
    UpdateConfig


{-| TODO
-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }



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
    -> Dropdown
    -> Maybe a
    -> Html (Msg a)
view (ViewConfig uniqueId views) ids allEntries (Dropdown data) maybeSelection =
    let
        buttonHtmlDetails =
            views.button
                { maybeSelection = maybeSelection
                , open = True
                }

        listboxConfig =
            Listbox.viewConfig uniqueId
                { ul =
                    if data.open then
                        Attributes.style "position" "absolute" :: views.ul
                    else
                        Attributes.style "display" "none"
                            :: Attributes.style "position" "absolute"
                            :: views.ul
                , liOption = views.liOption
                , liDivider = views.liDivider
                , empty = Html.text ""
                , focusable = True
                }
    in
    Html.div
        (appendAttributes views.container
            [ Events.onMouseDown ListboxMousePressed
            , Events.onMouseUp (ListboxMouseReleased ids.id)
            ]
        )
        [ viewButton ids.id buttonHtmlDetails ids.labelledBy maybeSelection True
        , Listbox.customViewUnique listboxConfig
            { id = printListboxId ids.id
            , labelledBy = ids.labelledBy
            , lift = ListboxMsg (Just ids.id)
            , onKeyDown =
                KeyInfo.decoder
                    |> Decode.andThen
                        (\{ code, altDown, controlDown, metaDown, shiftDown } ->
                            case code of
                                "Escape" ->
                                    if
                                        not altDown
                                            && not controlDown
                                            && not metaDown
                                            && not shiftDown
                                    then
                                        Decode.succeed (ListboxEscapePressed ids.id)
                                    else
                                        Decode.fail "not handling that key combination"

                                "Enter" ->
                                    if
                                        not altDown
                                            && not controlDown
                                            && not metaDown
                                            && not shiftDown
                                    then
                                        Decode.succeed (ListboxEnterPressed ids.id)
                                    else
                                        Decode.fail "not handling that key combination"

                                _ ->
                                    Decode.fail "not handling that key combination"
                        )
            , onMouseDown = Decode.fail "not handling this event here"
            , onMouseUp = Decode.fail "not handling this event here"
            , onBlur = Decode.succeed (ListboxBlured ids.id)
            }
            allEntries
            data.listbox
            maybeSelection
        ]


viewClosed : String -> HtmlAttributes -> HtmlDetails -> String -> Maybe a -> Html (Msg a)
viewClosed id containerHtmlAttributes buttonHtmlDetails labelledBy selection =
    Html.div
        (appendAttributes containerHtmlAttributes [])
        [ viewButton id buttonHtmlDetails labelledBy selection False ]


viewButton : String -> HtmlDetails -> String -> Maybe a -> Bool -> Html (Msg a)
viewButton id { attributes, children } labelledBy selection open =
    Html.button
        ([ Attributes.id (printButtonId id)
         , Attributes.type_ "button"
         , Attributes.attribute "aria-haspopup" "listbox"
         , Attributes.attribute "aria-labelledby"
            (printButtonId id ++ " " ++ labelledBy)
         , Attributes.style "position" "relative"
         , Attributes.tabindex 0
         , Events.onClick (ButtonClicked id)
         , Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen (buttonKeyDown id)
            )
         ]
            |> setAriaExpanded open
            |> appendAttributes attributes
        )
        (List.map (Html.map (\_ -> NoOp)) children)


buttonKeyDown : String -> String -> Decoder (Msg a)
buttonKeyDown id code =
    case code of
        "ArrowUp" ->
            Decode.succeed (ButtonArrowUpPressed id)

        "ArrowDown" ->
            Decode.succeed (ButtonArrowDownPressed id)

        _ ->
            Decode.fail "not handling that key here"



-- VIEW HELPER


setAriaExpanded : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setAriaExpanded isOpen attrs =
    if isOpen then
        Attributes.attribute "aria-expanded" "true" :: attrs
    else
        attrs



-- IDS


printButtonId : String -> String
printButtonId id =
    id ++ "__button"


printListboxId : String -> String
printListboxId id =
    id ++ "-listbox"



---- UPDATE


{-| TODO
-}
type Msg a
    = NoOp
    | NextAnimationFrame
      -- BUTTON
    | ButtonClicked String
    | ButtonArrowUpPressed String
    | ButtonArrowDownPressed String
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)
    | ListboxEscapePressed String
    | ListboxEnterPressed String
    | ListboxBlured String
    | ListboxMousePressed
    | ListboxMouseReleased String


{-| TODO
-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Dropdown
    -> Maybe a
    -> ( Dropdown, Cmd (Msg a), Maybe a )
update (UpdateConfig uniqueId behaviour) allEntries msg dropdown maybeSelection =
    let
        (Dropdown data) =
            dropdown

        listboxConfig =
            Listbox.updateConfig uniqueId
                { jumpAtEnds = behaviour.jumpAtEnds
                , separateFocus = behaviour.separateFocus
                , selectionFollowsFocus = behaviour.selectionFollowsFocus
                , handleHomeAndEnd = behaviour.handleHomeAndEnd
                , typeAhead = behaviour.typeAhead
                , minimalGap = behaviour.minimalGap
                , initialGap = behaviour.initialGap
                }
    in
    case msg of
        NoOp ->
            ( dropdown, Cmd.none, maybeSelection )

        NextAnimationFrame ->
            case data.pendingFocusListbox of
                Nothing ->
                    ( dropdown, Cmd.none, maybeSelection )

                Just id ->
                    ( Dropdown { data | pendingFocusListbox = Nothing }
                    , Task.attempt (\_ -> NoOp) <|
                        Listbox.focus (printListboxId id)
                    , maybeSelection
                    )

        -- BUTTON
        ButtonClicked id ->
            ( Dropdown { data | open = True }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , maybeSelection
            )

        ButtonArrowUpPressed id ->
            let
                ( newListbox, newSelection ) =
                    Listbox.withUnique maybeSelection <|
                        Listbox.focusPreviousOrFirstEntry listboxConfig allEntries data.listbox
            in
            ( Dropdown
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing
                        else
                            Just id
                }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , newSelection
            )

        ButtonArrowDownPressed id ->
            let
                ( newListbox, newSelection ) =
                    Listbox.withUnique maybeSelection <|
                        Listbox.focusNextOrFirstEntry listboxConfig allEntries data.listbox
            in
            ( Dropdown
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing
                        else
                            Just id
                }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , newSelection
            )

        ListboxMsg maybeId listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    Listbox.updateUnique listboxConfig
                        allEntries
                        listboxMsg
                        data.listbox
                        maybeSelection
            in
            ( Dropdown { data | listbox = newListbox }
            , Cmd.map (ListboxMsg maybeId) listboxCmd
            , newSelection
            )

        ListboxEscapePressed id ->
            ( Dropdown { data | open = False }
            , focusButton id
            , maybeSelection
            )

        ListboxEnterPressed id ->
            if data.open then
                ( Dropdown { data | open = False }
                , focusButton id
                , Listbox.focusedEntry listboxConfig data.listbox allEntries
                )
            else
                ( dropdown, Cmd.none, maybeSelection )

        ListboxBlured id ->
            if data.preventBlur then
                ( dropdown, Cmd.none, maybeSelection )
            else
                ( Dropdown { data | open = False }
                , focusButton id
                , maybeSelection
                )

        ListboxMousePressed ->
            ( Dropdown { data | preventBlur = True }
            , Cmd.none
            , maybeSelection
            )

        ListboxMouseReleased id ->
            if behaviour.closeAfterMouseSelection then
                ( Dropdown { data | open = False, preventBlur = False }
                , focusButton id
                , maybeSelection
                )
            else
                ( Dropdown { data | preventBlur = False }
                , Cmd.none
                , maybeSelection
                )



-- CMDS


focusButton : String -> Cmd (Msg a)
focusButton id =
    Dom.focus (printButtonId id)
        |> Task.attempt (\_ -> NoOp)



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : Dropdown -> Sub (Msg a)
subscriptions (Dropdown data) =
    Sub.batch
        [ if data.open then
            Sub.map (ListboxMsg Nothing) (Listbox.subscriptions data.listbox)
          else
            Sub.none
        ]



-- MISC


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


allowDefault : Decoder msg -> Decoder ( msg, Bool )
allowDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, False ))
