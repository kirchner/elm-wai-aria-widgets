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
import Widget.Listbox.SingleSelect as SingleSelect


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
    | ImportantFeaturesUpPressed Listbox.DomInfo
    | ImportantFeaturesAltArrowUpPressed Listbox.DomInfo
    | ImportantFeaturesDownPressed Listbox.DomInfo
    | ImportantFeaturesAltArrowDownPressed Listbox.DomInfo
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
                    SingleSelect.update listboxUpdateConfig
                        model.importantFeaturesListbox
                        (List.map Listbox.option model.importantFeatures)
                        model.selectedImportantFeature
                        listboxMsg
            in
            ( { model
                | importantFeaturesListbox = newListbox
                , selectedImportantFeature = newSelection
              }
            , Cmd.map ImportantFeaturesListboxMsg listboxCmd
            )

        ImportantFeaturesUpPressed domInfo ->
            moveImportantFeatureUp domInfo model

        ImportantFeaturesAltArrowUpPressed domInfo ->
            moveImportantFeatureUp domInfo model

        ImportantFeaturesDownPressed domInfo ->
            moveImportantFeatureDown domInfo model

        ImportantFeaturesAltArrowDownPressed domInfo ->
            moveImportantFeatureDown domInfo model

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
                                    SingleSelect.focusEntry listboxUpdateConfig
                                        newFeature
                                        model.selectedImportantFeature
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
                    SingleSelect.update listboxUpdateConfig
                        model.unimportantFeaturesListbox
                        (List.map Listbox.option model.unimportantFeatures)
                        model.selectedUnimportantFeature
                        listboxMsg
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
                                    SingleSelect.focusEntry listboxUpdateConfig
                                        newFeature
                                        model.selectedUnimportantFeature
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


moveImportantFeatureUp : Listbox.DomInfo -> Model -> ( Model, Cmd Msg )
moveImportantFeatureUp domInfo model =
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
            , Task.attempt (\_ -> NoOp) <|
                Listbox.scrollIntoViewVia domInfo
                    "important-features-listbox"
                    model.importantFeaturesListbox
            )


moveImportantFeatureDown : Listbox.DomInfo -> Model -> ( Model, Cmd Msg )
moveImportantFeatureDown domInfo model =
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
            , Task.attempt (\_ -> NoOp) <|
                Listbox.scrollIntoViewVia domInfo
                    "important-features-listbox"
                    model.importantFeaturesListbox
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

        aboveFocused =
            Listbox.fromFocused -1
                listboxViewConfig
                model.importantFeaturesListbox
                entries

        belowFocused =
            Listbox.fromFocused 1
                listboxViewConfig
                model.importantFeaturesListbox
                entries

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
                    [ Listbox.customView
                        listboxViewConfig
                        { id = "important-features-listbox"
                        , labelledBy = "important-features-listbox-label"
                        , lift = ImportantFeaturesListboxMsg
                        , onKeyPress =
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
                                                    Listbox.domInfoOf aboveFocused [ "target" ]
                                                        |> Decode.map ImportantFeaturesAltArrowUpPressed

                                            ( "ArrowDown", True ) ->
                                                if lastSelected then
                                                    Decode.fail "not handling that key here"
                                                else
                                                    Listbox.domInfoOf belowFocused [ "target" ]
                                                        |> Decode.map ImportantFeaturesAltArrowDownPressed

                                            ( "Delete", False ) ->
                                                Decode.succeed ImportantFeaturesDeletePressed

                                            _ ->
                                                Decode.fail "not handling that key here"
                                    )
                        , onMouseDown = Decode.fail "not handling this event here"
                        , onMouseUp = Decode.fail "not handling this event here"
                        , onBlur = Decode.fail "not handling this event here"
                        }
                        model.importantFeaturesListbox
                        entries
                        (maybeToList model.selectedImportantFeature)
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
                            Decode.map ImportantFeaturesUpPressed <|
                                Listbox.domInfoOf aboveFocused pathToListbox
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
                            Decode.map ImportantFeaturesDownPressed <|
                                Listbox.domInfoOf belowFocused pathToListbox
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
                    [ Listbox.customView listboxViewConfig
                        { id = "unimportant-features-listbox"
                        , labelledBy = "unimportant-features-listbox-label"
                        , lift = UnimportantFeaturesListboxMsg
                        , onKeyPress =
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
                        model.unimportantFeaturesListbox
                        (List.map Listbox.option model.unimportantFeatures)
                        (maybeToList model.selectedUnimportantFeature)
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


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]


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
