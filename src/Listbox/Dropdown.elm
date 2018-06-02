module Listbox.Dropdown
    exposing
        ( Behaviour
        , Config
        , Dropdown
        , HtmlAttributes
        , HtmlDetails
        , Ids
        , Msg
        , View
        , closed
        , subscriptions
        , update
        , view
        )

{-|

@docs Dropdown, closed, view, Ids, update, Msg, subscriptions


# Configuration

@docs Config, Behaviour, View

@docs HtmlAttributes, HtmlDetails

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
import Internal.Entries
    exposing
        ( Next(..)
        , Previous(..)
        , findNext
        , findPrevious
        )
import Json.Decode as Decode exposing (Decoder)
import Listbox exposing (Listbox, TypeAhead)
import Task


{-| TODO
-}
type Dropdown
    = Closed
    | Open OpenData


type alias OpenData =
    { preventBlur : Bool
    , listbox : Listbox
    }


{-| TODO
-}
closed : Dropdown
closed =
    Closed



---- CONFIG


{-| TODO
-}
type alias Config a =
    { uniqueId : a -> String
    , behaviour : Behaviour a
    , view : View a
    }


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


{-| TODO
-}
type alias View a =
    { container : HtmlAttributes
    , button :
        { maybeSelection : Maybe a
        , open : Bool
        }
        -> HtmlDetails
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



---- VIEW


{-| TODO
-}
type alias Ids =
    { id : String
    , labelledBy : String
    }


{-| TODO
-}
view : Config a -> Ids -> Dropdown -> List a -> Maybe a -> Html (Msg a)
view config ids state allEntries maybeSelection =
    let
        data =
            { behaviour = config.behaviour
            , id = ids.id
            , uniqueId = config.uniqueId
            }

        buttonHtmlDetails =
            config.view.button
                { maybeSelection = maybeSelection
                , open = True
                }
    in
    case state of
        Closed ->
            viewClosed data config.view.container buttonHtmlDetails ids.labelledBy maybeSelection

        Open { listbox } ->
            let
                listboxConfig =
                    { uniqueId = config.uniqueId
                    , behaviour =
                        { jumpAtEnds = config.behaviour.jumpAtEnds
                        , separateFocus = config.behaviour.separateFocus
                        , selectionFollowsFocus = config.behaviour.selectionFollowsFocus
                        , handleHomeAndEnd = config.behaviour.handleHomeAndEnd
                        , typeAhead = config.behaviour.typeAhead
                        }
                    , view =
                        { ul =
                            Attributes.style "position" "absolute" :: config.view.ul
                        , li = config.view.li
                        , empty = Html.div [] []
                        }
                    }

                selection =
                    case maybeSelection of
                        Nothing ->
                            []

                        Just actualSelection ->
                            [ actualSelection ]
            in
            Html.div
                (appendAttributes config.view.container [])
                [ viewButton data buttonHtmlDetails ids.labelledBy maybeSelection True
                , Listbox.view listboxConfig
                    { id = printListboxId ids.id
                    , labelledBy = ids.labelledBy
                    }
                    listbox
                    allEntries
                    selection
                    |> Html.map (ListboxMsg (Just ids.id))
                ]


viewClosed : Data a -> HtmlAttributes -> HtmlDetails -> String -> Maybe a -> Html (Msg a)
viewClosed data containerHtmlAttributes buttonHtmlDetails labelledBy selection =
    Html.div
        (appendAttributes containerHtmlAttributes [])
        [ viewButton data buttonHtmlDetails labelledBy selection False ]


viewButton : Data a -> HtmlDetails -> String -> Maybe a -> Bool -> Html (Msg a)
viewButton ({ id } as data) { attributes, children } labelledBy selection open =
    Html.button
        ([ Attributes.id (printButtonId id)
         , Attributes.type_ "button"
         , Attributes.attribute "aria-haspopup" "listbox"
         , Attributes.attribute "aria-labelledby"
            (printButtonId id ++ " " ++ labelledBy)
         , Attributes.style "position" "relative"
         , Attributes.tabindex 0
         , Events.onClick (ButtonClicked data)
         , Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen (buttonKeyDown data)
            )
         ]
            |> setAriaExpanded open
            |> appendAttributes attributes
        )
        (List.map (Html.map (\_ -> NoOp)) children)


buttonKeyDown : Data a -> String -> Decoder (Msg a)
buttonKeyDown data code =
    case code of
        "ArrowUp" ->
            Decode.succeed (ButtonArrowUpPressed data)

        "ArrowDown" ->
            Decode.succeed (ButtonArrowDownPressed data)

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
      -- BUTTON
    | ButtonClicked (Data a)
    | ButtonArrowUpPressed (Data a)
    | ButtonArrowDownPressed (Data a)
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)


type alias Data a =
    { behaviour : Behaviour a
    , id : String
    , uniqueId : a -> String
    }


{-| TODO
-}
update :
    (a -> outMsg)
    -> Dropdown
    -> List a
    -> Maybe a
    -> Msg a
    -> ( Dropdown, Cmd (Msg a), Maybe outMsg )
update entrySelected state allEntries maybeSelection msg =
    case state of
        Closed ->
            updateClosed entrySelected allEntries maybeSelection msg

        Open stuff ->
            updateOpen entrySelected allEntries maybeSelection stuff msg


updateClosed :
    (a -> outMsg)
    -> List a
    -> Maybe a
    -> Msg a
    -> ( Dropdown, Cmd (Msg a), Maybe outMsg )
updateClosed entrySelected allEntries maybeSelection msg =
    case msg of
        NoOp ->
            ( Closed, Cmd.none, Nothing )

        -- BUTTON
        ButtonClicked { behaviour, id, uniqueId } ->
            case maybeSelection of
                Nothing ->
                    case List.head allEntries of
                        Nothing ->
                            ( Closed, Cmd.none, Nothing )

                        Just firstEntry ->
                            let
                                ( listbox, listboxCmd ) =
                                    Listbox.focused (printListboxId id) (uniqueId firstEntry)
                            in
                            ( Open
                                { preventBlur = False
                                , listbox = listbox
                                }
                            , Cmd.map (ListboxMsg (Just id)) listboxCmd
                            , if behaviour.selectionFollowsFocus then
                                Just (entrySelected firstEntry)
                              else
                                Nothing
                            )

                Just selection ->
                    if List.member selection allEntries then
                        let
                            ( listbox, listboxCmd ) =
                                Listbox.focused (printListboxId id) (uniqueId selection)
                        in
                        ( Open
                            { preventBlur = False
                            , listbox = listbox
                            }
                        , Cmd.map (ListboxMsg (Just id)) listboxCmd
                        , Nothing
                        )
                    else
                        ( Closed, Cmd.none, Nothing )

        ButtonArrowUpPressed { behaviour, id, uniqueId } ->
            case maybeSelection of
                Nothing ->
                    case List.head (List.reverse allEntries) of
                        Nothing ->
                            ( Closed, Cmd.none, Nothing )

                        Just lastEntry ->
                            let
                                ( listbox, listboxCmd ) =
                                    Listbox.focused (printListboxId id) (uniqueId lastEntry)
                            in
                            ( Open
                                { preventBlur = False
                                , listbox = listbox
                                }
                            , Cmd.map (ListboxMsg (Just id)) listboxCmd
                            , if behaviour.selectionFollowsFocus then
                                Just (entrySelected lastEntry)
                              else
                                Nothing
                            )

                Just selection ->
                    case findPrevious uniqueId (uniqueId selection) allEntries of
                        Just (Last lastEntry) ->
                            if behaviour.jumpAtEnds then
                                let
                                    ( listbox, listboxCmd ) =
                                        Listbox.focused (printListboxId id) (uniqueId lastEntry)
                                in
                                ( Open
                                    { preventBlur = False
                                    , listbox = listbox
                                    }
                                , Cmd.map (ListboxMsg (Just id)) listboxCmd
                                , if behaviour.selectionFollowsFocus then
                                    Just (entrySelected lastEntry)
                                  else
                                    Nothing
                                )
                            else
                                let
                                    ( listbox, listboxCmd ) =
                                        Listbox.focused (printListboxId id) (uniqueId selection)
                                in
                                ( Open
                                    { preventBlur = False
                                    , listbox = listbox
                                    }
                                , Cmd.map (ListboxMsg (Just id)) listboxCmd
                                , Nothing
                                )

                        Just (Previous newIndex newEntry) ->
                            let
                                ( listbox, listboxCmd ) =
                                    Listbox.focused (printListboxId id) (uniqueId newEntry)
                            in
                            ( Open
                                { preventBlur = False
                                , listbox = listbox
                                }
                            , Cmd.map (ListboxMsg (Just id)) listboxCmd
                            , if behaviour.selectionFollowsFocus then
                                Just (entrySelected newEntry)
                              else
                                Nothing
                            )

                        Nothing ->
                            ( Closed, Cmd.none, Nothing )

        ButtonArrowDownPressed { behaviour, id, uniqueId } ->
            case maybeSelection of
                Nothing ->
                    case List.head allEntries of
                        Nothing ->
                            ( Closed, Cmd.none, Nothing )

                        Just firstEntry ->
                            let
                                ( listbox, listboxCmd ) =
                                    Listbox.focused (printListboxId id) (uniqueId firstEntry)
                            in
                            ( Open
                                { preventBlur = False
                                , listbox = listbox
                                }
                            , Cmd.map (ListboxMsg (Just id)) listboxCmd
                            , if behaviour.selectionFollowsFocus then
                                Just (entrySelected firstEntry)
                              else
                                Nothing
                            )

                Just selection ->
                    case findNext uniqueId (uniqueId selection) allEntries of
                        Just (First firstEntry) ->
                            if behaviour.jumpAtEnds then
                                let
                                    ( listbox, listboxCmd ) =
                                        Listbox.focused (printListboxId id) (uniqueId firstEntry)
                                in
                                ( Open
                                    { preventBlur = False
                                    , listbox = listbox
                                    }
                                , Cmd.map (ListboxMsg (Just id)) listboxCmd
                                , if behaviour.selectionFollowsFocus then
                                    Just (entrySelected firstEntry)
                                  else
                                    Nothing
                                )
                            else
                                let
                                    ( listbox, listboxCmd ) =
                                        Listbox.focused (printListboxId id) (uniqueId selection)
                                in
                                ( Open
                                    { preventBlur = False
                                    , listbox = listbox
                                    }
                                , Cmd.map (ListboxMsg (Just id)) listboxCmd
                                , Nothing
                                )

                        Just (Next newIndex newEntry) ->
                            let
                                ( listbox, listboxCmd ) =
                                    Listbox.focused (printListboxId id) (uniqueId newEntry)
                            in
                            ( Open
                                { preventBlur = False
                                , listbox = listbox
                                }
                            , Cmd.map (ListboxMsg (Just id)) listboxCmd
                            , if behaviour.selectionFollowsFocus then
                                Just (entrySelected newEntry)
                              else
                                Nothing
                            )

                        Nothing ->
                            ( Closed, Cmd.none, Nothing )

        _ ->
            ( Closed, Cmd.none, Nothing )


type OutMsg a
    = EntrySelected a
    | ListboxBlured
    | ListboxEscapePressed


updateOpen :
    (a -> outMsg)
    -> List a
    -> Maybe a
    -> OpenData
    -> Msg a
    -> ( Dropdown, Cmd (Msg a), Maybe outMsg )
updateOpen entrySelected allEntries maybeSelection stuff msg =
    case msg of
        ListboxMsg maybeId listboxMsg ->
            let
                selection =
                    case maybeSelection of
                        Nothing ->
                            []

                        Just actualSelection ->
                            [ actualSelection ]

                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update
                        [ Listbox.onEntrySelect EntrySelected
                        , Listbox.onListboxBlur ListboxBlured
                        , Listbox.onEscapeDown ListboxEscapePressed
                        ]
                        stuff.listbox
                        allEntries
                        selection
                        listboxMsg

                newDropdown =
                    Open
                        { stuff | listbox = newListbox }

                dropdownCmd =
                    Cmd.map (ListboxMsg maybeId) listboxCmd
            in
            case maybeOutMsg of
                Nothing ->
                    ( newDropdown
                    , dropdownCmd
                    , Nothing
                    )

                Just ListboxBlured ->
                    ( Closed
                    , dropdownCmd
                    , Nothing
                    )

                Just (EntrySelected a) ->
                    ( Closed
                    , Cmd.batch
                        [ dropdownCmd
                        , maybeId
                            |> Maybe.map focusButton
                            |> Maybe.withDefault Cmd.none
                        ]
                    , Just (entrySelected a)
                    )

                Just ListboxEscapePressed ->
                    ( Closed
                    , Cmd.batch
                        [ dropdownCmd
                        , maybeId
                            |> Maybe.map focusButton
                            |> Maybe.withDefault Cmd.none
                        ]
                    , Nothing
                    )

        _ ->
            ( Open stuff, Cmd.none, Nothing )



-- CMDS


focusButton : String -> Cmd (Msg a)
focusButton id =
    Browser.focus (printButtonId id)
        |> Task.attempt (\_ -> NoOp)



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : Dropdown -> Sub (Msg a)
subscriptions dropdown =
    case dropdown of
        Closed ->
            Sub.none

        Open { listbox } ->
            Sub.map (ListboxMsg Nothing) (Listbox.subscriptions listbox)



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
