module Widget.Listbox.Dropdown
    exposing
        ( Behaviour
        , Dropdown
        , Ids
        , Msg
        , UpdateConfig
        , ViewConfig
        , Views
        , closed
        , subscriptions
        , update
        , updateConfig
        , view
        , viewConfig
        )

{-|

@docs Dropdown, closed, view, Ids, update, Msg, subscriptions


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

import AnimationFrame
import Browser
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
closed : Dropdown
closed =
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
    }



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
    -> Dropdown
    -> List (Entry a divider)
    -> Maybe a
    -> Html (Msg a)
view (ViewConfig uniqueId views) ids (Dropdown data) allEntries maybeSelection =
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

        selection =
            case maybeSelection of
                Nothing ->
                    []

                Just actualSelection ->
                    [ actualSelection ]
    in
    Html.div
        (appendAttributes views.container
            [ Events.on "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "Enter" ->
                                    Decode.succeed (ContainerEnterPressed ids.id)

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )
            ]
        )
        [ viewButton ids.id buttonHtmlDetails ids.labelledBy maybeSelection True
        , Listbox.view listboxConfig
            { id = printListboxId ids.id
            , labelledBy = ids.labelledBy
            , lift = ListboxMsg (Just ids.id)
            , onKeyDown =
                Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\rawCode ->
                            case rawCode of
                                "Escape" ->
                                    Decode.succeed (ListboxEscapePressed ids.id)

                                _ ->
                                    Decode.fail "not handling keys here"
                        )
            }
            data.listbox
            allEntries
            selection
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
      -- CONTAINER
    | ContainerEnterPressed String
      -- BUTTON
    | ButtonClicked String
    | ButtonArrowUpPressed String
    | ButtonArrowDownPressed String
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)
    | ListboxEscapePressed String


type OutMsg a
    = EntrySelected a
    | ListboxBlured


{-| TODO
-}
update :
    UpdateConfig a
    -> (a -> outMsg)
    -> Dropdown
    -> List (Entry a divider)
    -> Maybe a
    -> Msg a
    -> ( Dropdown, Cmd (Msg a), Maybe outMsg )
update (UpdateConfig uniqueId behaviour) entrySelected (Dropdown data) allEntries maybeSelection msg =
    let
        listboxConfig =
            Listbox.updateConfig uniqueId
                { jumpAtEnds = behaviour.jumpAtEnds
                , separateFocus = behaviour.separateFocus
                , selectionFollowsFocus = behaviour.selectionFollowsFocus
                , handleHomeAndEnd = behaviour.handleHomeAndEnd
                , typeAhead = behaviour.typeAhead
                }
    in
    case msg of
        NoOp ->
            ( Dropdown data, Cmd.none, Nothing )

        NextAnimationFrame ->
            case data.pendingFocusListbox of
                Nothing ->
                    ( Dropdown data, Cmd.none, Nothing )

                Just id ->
                    ( Dropdown { data | pendingFocusListbox = Nothing }
                    , Task.attempt (\_ -> NoOp) <|
                        Listbox.focus (printListboxId id)
                    , Nothing
                    )

        -- CONTAINER
        ContainerEnterPressed id ->
            if data.open then
                ( Dropdown { data | open = False }
                , focusButton id
                , Nothing
                )
            else
                ( Dropdown data, Cmd.none, Nothing )

        -- BUTTON
        ButtonClicked id ->
            ( Dropdown { data | open = True }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , Nothing
            )

        ButtonArrowUpPressed id ->
            let
                ( newListbox, maybeOutMsg ) =
                    Listbox.focusPreviousOrFirstEntry listboxConfig allEntries data.listbox
            in
            handleOutMsg
                entrySelected
                (Just id)
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing
                        else
                            Just id
                }
                Cmd.none
                maybeOutMsg

        ButtonArrowDownPressed id ->
            let
                ( newListbox, maybeOutMsg ) =
                    Listbox.focusNextOrFirstEntry listboxConfig allEntries data.listbox
            in
            handleOutMsg
                entrySelected
                (Just id)
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing
                        else
                            Just id
                }
                Cmd.none
                maybeOutMsg

        ListboxMsg maybeId listboxMsg ->
            let
                selection =
                    case maybeSelection of
                        Nothing ->
                            []

                        Just actualSelection ->
                            [ actualSelection ]

                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update listboxConfig
                        [ Listbox.onEntrySelect EntrySelected
                        , Listbox.onListboxBlur ListboxBlured
                        ]
                        data.listbox
                        allEntries
                        selection
                        listboxMsg
            in
            handleOutMsg
                entrySelected
                maybeId
                { data | listbox = newListbox }
                (Cmd.map (ListboxMsg maybeId) listboxCmd)
                maybeOutMsg

        ListboxEscapePressed id ->
            ( Dropdown { data | open = False }
            , focusButton id
            , Nothing
            )


handleOutMsg :
    (a -> outMsg)
    -> Maybe String
    -> Data
    -> Cmd (Msg a)
    -> Maybe (OutMsg a)
    -> ( Dropdown, Cmd (Msg a), Maybe outMsg )
handleOutMsg entrySelected maybeId data cmd maybeOutMsg =
    case maybeOutMsg of
        Nothing ->
            ( Dropdown data, cmd, Nothing )

        Just ListboxBlured ->
            ( Dropdown { data | open = False }
            , cmd
            , Nothing
            )

        Just (EntrySelected a) ->
            ( Dropdown data
            , cmd
            , Just (entrySelected a)
            )



-- CMDS


focusButton : String -> Cmd (Msg a)
focusButton id =
    Browser.focus (printButtonId id)
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
        , case data.pendingFocusListbox of
            Nothing ->
                Sub.none

            Just _ ->
                AnimationFrame.times (\_ -> NextAnimationFrame)
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
