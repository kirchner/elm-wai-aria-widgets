module Listboxes.SingleSelect
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

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import List.Extra as List
import Task
import Widget.Listbox as Listbox exposing (Listbox)


type alias Model =
    { importantFeaturesListbox : Listbox
    , importantFeatures : List String
    , selectedImportantFeature : Maybe String
    , unimportantFeaturesListbox : Listbox
    , unimportantFeatures : List String
    , selectedUnimportantFeature : Maybe String
    }


init : Model
init =
    { importantFeaturesListbox = Listbox.init
    , importantFeatures =
        [ "Proximity of public K-12 schools"
        , "Proximity of child-friendly parks"
        , "Proximity of grocery shopping"
        , "Proximity of fast food"
        , "Proximity of fine dining"
        , "Neighborhood walkability"
        , "Availability of public transit"
        , "Proximity of hospital and medical services"
        , "Level of traffic noise"
        , "Access to major highways"
        ]
    , selectedImportantFeature = Nothing
    , unimportantFeaturesListbox = Listbox.init
    , unimportantFeatures = []
    , selectedUnimportantFeature = Nothing
    }



---- UPDATE


type Msg
    = NoOp
    | ImportantFeaturesListboxMsg (Listbox.Msg String)
    | ImportantFeaturesUpPressed
    | ImportantFeaturesAltArrowUpPressed
    | ImportantFeaturesDownPressed
    | ImportantFeaturesAltArrowDownPressed
    | ImportantFeaturesNotImportantClicked
    | ImportantFeaturesDeletePressed
    | UnimportantFeaturesListboxMsg (Listbox.Msg String)
    | UnimportantFeaturesImportantClicked
    | UnimportantFeaturesEnterPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ImportantFeaturesListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    Listbox.updateUnique listboxUpdateConfig
                        (List.map Listbox.option model.importantFeatures)
                        listboxMsg
                        model.importantFeaturesListbox
                        model.selectedImportantFeature
            in
            ( { model
                | importantFeaturesListbox = newListbox
                , selectedImportantFeature = newSelection
              }
            , Cmd.map ImportantFeaturesListboxMsg listboxCmd
            )

        ImportantFeaturesUpPressed ->
            moveImportantFeatureUp model

        ImportantFeaturesAltArrowUpPressed ->
            moveImportantFeatureUp model

        ImportantFeaturesDownPressed ->
            moveImportantFeatureDown model

        ImportantFeaturesAltArrowDownPressed ->
            moveImportantFeatureDown model

        ImportantFeaturesNotImportantClicked ->
            case model.selectedImportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    ( { model
                        | selectedImportantFeature = Nothing
                        , importantFeatures =
                            List.filter (\f -> f /= feature) model.importantFeatures
                        , unimportantFeatures = feature :: model.unimportantFeatures
                      }
                    , Cmd.none
                    )

        ImportantFeaturesDeletePressed ->
            case model.selectedImportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    let
                        ( start, end ) =
                            model.importantFeatures
                                |> List.break (\f -> f == feature)

                        ( newListbox, newSelection ) =
                            case
                                List.head (List.drop 1 end)
                                    |> or (List.head (List.reverse start))
                            of
                                Nothing ->
                                    ( model.importantFeaturesListbox
                                    , model.selectedImportantFeature
                                    )

                                Just newFeature ->
                                    Listbox.withUnique model.selectedImportantFeature <|
                                        Listbox.focusEntry listboxUpdateConfig
                                            newFeature
                                            model.importantFeaturesListbox
                    in
                    ( { model
                        | importantFeaturesListbox = newListbox
                        , selectedImportantFeature = newSelection
                        , importantFeatures = start ++ List.drop 1 end
                        , unimportantFeatures = feature :: model.unimportantFeatures
                      }
                    , Cmd.none
                    )

        UnimportantFeaturesListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    Listbox.updateUnique listboxUpdateConfig
                        (List.map Listbox.option model.unimportantFeatures)
                        listboxMsg
                        model.unimportantFeaturesListbox
                        model.selectedUnimportantFeature
            in
            ( { model
                | unimportantFeaturesListbox = newListbox
                , selectedUnimportantFeature = newSelection
              }
            , Cmd.map UnimportantFeaturesListboxMsg listboxCmd
            )

        UnimportantFeaturesImportantClicked ->
            case model.selectedUnimportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    ( { model
                        | selectedUnimportantFeature = Nothing
                        , unimportantFeatures =
                            List.filter (\f -> f /= feature) model.unimportantFeatures
                        , importantFeatures = feature :: model.importantFeatures
                      }
                    , Cmd.none
                    )

        UnimportantFeaturesEnterPressed ->
            case model.selectedUnimportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    let
                        ( start, end ) =
                            model.unimportantFeatures
                                |> List.break (\f -> f == feature)

                        ( newListbox, newSelection ) =
                            case
                                List.head (List.drop 1 end)
                                    |> or (List.head (List.reverse start))
                            of
                                Nothing ->
                                    ( model.unimportantFeaturesListbox
                                    , model.selectedUnimportantFeature
                                    )

                                Just newFeature ->
                                    Listbox.withUnique model.selectedUnimportantFeature <|
                                        Listbox.focusEntry listboxUpdateConfig
                                            newFeature
                                            model.unimportantFeaturesListbox
                    in
                    ( { model
                        | unimportantFeaturesListbox = newListbox
                        , selectedUnimportantFeature = newSelection
                        , unimportantFeatures = start ++ List.drop 1 end
                        , importantFeatures = feature :: model.importantFeatures
                      }
                    , Cmd.none
                    )


moveImportantFeatureUp : Model -> ( Model, Cmd Msg )
moveImportantFeatureUp model =
    case model.selectedImportantFeature of
        Nothing ->
            ( model, Cmd.none )

        Just feature ->
            let
                moveUp features =
                    case features of
                        current :: next :: rest ->
                            if next == feature then
                                next :: current :: moveUp rest
                            else
                                current :: moveUp (next :: rest)

                        _ ->
                            features
            in
            ( { model | importantFeatures = moveUp model.importantFeatures }
            , Cmd.map ImportantFeaturesListboxMsg <|
                Listbox.scrollToFocus "important-features-listbox" model.importantFeaturesListbox
            )


moveImportantFeatureDown : Model -> ( Model, Cmd Msg )
moveImportantFeatureDown model =
    case model.selectedImportantFeature of
        Nothing ->
            ( model, Cmd.none )

        Just feature ->
            let
                moveDown features =
                    case features of
                        current :: next :: rest ->
                            if current == feature then
                                next :: current :: moveDown rest
                            else
                                current :: moveDown (next :: rest)

                        _ ->
                            features
            in
            ( { model | importantFeatures = moveDown model.importantFeatures }
            , Cmd.map ImportantFeaturesListboxMsg <|
                Listbox.scrollToFocus "important-features-listbox" model.importantFeaturesListbox
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ImportantFeaturesListboxMsg
            (Listbox.subscriptions model.importantFeaturesListbox)
        , Sub.map UnimportantFeaturesListboxMsg
            (Listbox.subscriptions model.unimportantFeaturesListbox)
        ]



---- VIEW


view : Model -> Html Msg
view model =
    let
        firstSelected =
            List.head model.importantFeatures == model.selectedImportantFeature

        lastSelected =
            List.head (List.reverse model.importantFeatures)
                == model.selectedImportantFeature

        entries =
            List.map Listbox.option model.importantFeatures

        pathToListbox =
            [ "target"
            , "parentElement"
            , "parentElement"
            , "previousSibling"
            , "childNodes"
            , "1"
            , "firstChild"
            ]
    in
    Html.div
        [ Attributes.class "columns" ]
        [ Html.div
            [ Attributes.class "column" ]
            [ Html.div
                [ Attributes.class "field" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.id "important-features-listbox-label"
                    ]
                    [ Html.text "Important features:" ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Listbox.customViewUnique listboxViewConfig
                        { id = "important-features-listbox"
                        , labelledBy = "important-features-listbox-label"
                        , lift = ImportantFeaturesListboxMsg
                        , onKeyDown =
                            Decode.map2 Tuple.pair
                                (Decode.field "key" Decode.string)
                                (Decode.field "altKey" Decode.bool)
                                |> Decode.andThen
                                    (\( rawCode, altDown ) ->
                                        case ( rawCode, altDown ) of
                                            ( "ArrowUp", True ) ->
                                                if firstSelected then
                                                    Decode.fail "not handling that key here"
                                                else
                                                    Decode.succeed ImportantFeaturesAltArrowUpPressed

                                            ( "ArrowDown", True ) ->
                                                if lastSelected then
                                                    Decode.fail "not handling that key here"
                                                else
                                                    Decode.succeed ImportantFeaturesAltArrowDownPressed

                                            ( "Delete", False ) ->
                                                Decode.succeed ImportantFeaturesDeletePressed

                                            _ ->
                                                Decode.fail "not handling that key here"
                                    )
                        , onMouseDown = Decode.fail "not handling this event here"
                        , onMouseUp = Decode.fail "not handling this event here"
                        , onBlur = Decode.fail "not handling this event here"
                        }
                        entries
                        model.importantFeaturesListbox
                        model.selectedImportantFeature
                    ]
                ]
            , Html.div
                [ Attributes.class "field"
                , Attributes.class "is-grouped"
                , Attributes.attribute "role" "toolbar"
                , Attributes.attribute "aria-label" "Actions"
                ]
                [ Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled
                            (model.selectedImportantFeature == Nothing || firstSelected)
                        , Events.on "click" <|
                            Decode.succeed ImportantFeaturesUpPressed
                        , Attributes.attribute "aria-keyshortcuts" "Alt+ArrowUp"
                        , Attributes.attribute "aria-disabled" <|
                            boolToString (model.selectedImportantFeature == Nothing || firstSelected)
                        ]
                        [ Html.text "Up" ]
                    ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled
                            (model.selectedImportantFeature == Nothing || lastSelected)
                        , Events.on "click" <|
                            Decode.succeed ImportantFeaturesDownPressed
                        , Attributes.attribute "aria-keyshortcuts" "Alt+ArrowDown"
                        , Attributes.attribute "aria-disabled" <|
                            boolToString (model.selectedImportantFeature == Nothing || lastSelected)
                        ]
                        [ Html.text "Down" ]
                    ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled (model.selectedImportantFeature == Nothing)
                        , Events.onClick ImportantFeaturesNotImportantClicked
                        , Attributes.attribute "aria-keyshortcuts" "Delete"
                        , Attributes.attribute "aria-disabled" <|
                            boolToString (model.selectedImportantFeature == Nothing)
                        ]
                        [ Html.span []
                            [ Html.text "Not Important " ]
                        , Html.span
                            [ Attributes.class "icon"
                            , Attributes.style "font-size" "12px"
                            , Attributes.class "is-small"
                            ]
                            [ Html.i
                                [ Attributes.class "fas"
                                , Attributes.class "fa-arrow-right"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        , Html.div
            [ Attributes.class "column" ]
            [ Html.div
                [ Attributes.class "field" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.id "unimportant-features-listbox-label"
                    ]
                    [ Html.text "Unimportant features:" ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Listbox.customViewUnique listboxViewConfig
                        { id = "unimportant-features-listbox"
                        , labelledBy = "unimportant-features-listbox-label"
                        , lift = UnimportantFeaturesListboxMsg
                        , onKeyDown =
                            Decode.field "key" Decode.string
                                |> Decode.andThen
                                    (\rawCode ->
                                        case rawCode of
                                            "Enter" ->
                                                Decode.succeed UnimportantFeaturesEnterPressed

                                            _ ->
                                                Decode.fail "not handling that key here"
                                    )
                        , onMouseDown = Decode.fail "not handling this event here"
                        , onMouseUp = Decode.fail "not handling this event here"
                        , onBlur = Decode.fail "not handling this event here"
                        }
                        (List.map Listbox.option model.unimportantFeatures)
                        model.unimportantFeaturesListbox
                        model.selectedUnimportantFeature
                    ]
                ]
            , Html.div
                [ Attributes.class "field"
                , Attributes.class "is-grouped"
                ]
                [ Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled (model.selectedUnimportantFeature == Nothing)
                        , Events.onClick UnimportantFeaturesImportantClicked
                        , Attributes.attribute "aria-keyshortcuts" "Enter"
                        , Attributes.attribute "aria-disabled" <|
                            boolToString (model.selectedUnimportantFeature == Nothing)
                        ]
                        [ Html.span
                            [ Attributes.class "icon"
                            , Attributes.style "font-size" "12px"
                            , Attributes.class "is-small"
                            ]
                            [ Html.i
                                [ Attributes.class "fas"
                                , Attributes.class "fa-arrow-left"
                                ]
                                []
                            ]
                        , Html.span []
                            [ Html.text " Important" ]
                        ]
                    ]
                ]
            ]
        ]



---- CONFIGURATION


listboxUpdateConfig : Listbox.UpdateConfig String
listboxUpdateConfig =
    Listbox.updateConfig identity
        { jumpAtEnds = False
        , separateFocus = True
        , selectionFollowsFocus = True
        , handleHomeAndEnd = True
        , typeAhead = Listbox.noTypeAhead
        }


listboxViewConfig : Listbox.ViewConfig String Never
listboxViewConfig =
    Listbox.viewConfig identity
        { ul = [ Attributes.class "list" ]
        , liOption =
            \{ keyboardFocused, mouseFocused, maybeQuery } name ->
                { attributes =
                    [ Attributes.class "entry"
                    , Attributes.classList
                        [ ( "entry--keyboard-focused", keyboardFocused )
                        , ( "entry--mouse-focused", mouseFocused )
                        ]
                    ]
                , children = [ Html.text name ]
                }
        , liDivider = Listbox.noDivider
        , empty = Html.div [] [ Html.text "this list is empty" ]
        , focusable = True
        }



---- HELPER


or : Maybe a -> Maybe a -> Maybe a
or fallback default =
    case default of
        Nothing ->
            fallback

        Just _ ->
            default


boolToString : Bool -> String
boolToString b =
    if b then
        "true"
    else
        "false"
