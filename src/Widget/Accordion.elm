module Widget.Accordion
    exposing
        ( Accordion
        , Behaviour
        , Count(..)
        , PanelState(..)
        , Section
        , ViewConfig
        , Views
        , init
        , section
        , view
        , viewConfig
        )

{-|

@docs Accordion, init, view, section, Section, PanelState


# Configuration

@docs ViewConfig, viewConfig, Behaviour, Count, Views

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
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Task
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| TODO
-}
type Accordion
    = Accordion (Dict String PanelState)


{-| TODO
-}
init : Accordion
init =
    Accordion Dict.empty


{-| TODO
-}
type PanelState
    = Collapsed
    | Expanded


{-| TODO
-}
type Section header msg
    = Section PanelState (SectionData header msg)


type alias SectionData header msg =
    { id : String
    , header : header
    , panel : List (Html msg)
    }


{-| TODO
-}
section :
    PanelState
    ->
        { id : String
        , header : header
        , panel : List (Html msg)
        }
    -> Section header msg
section =
    Section



---- CONFIG


{-| TODO
-}
type ViewConfig header
    = ViewConfig Behaviour (Views header)


{-| TODO
-}
viewConfig : Behaviour -> Views header -> ViewConfig header
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views header =
    { dl : HtmlAttributes
    , dt : HtmlAttributes
    , button : PanelState -> header -> HtmlDetails
    , dd : HtmlAttributes
    }


{-| TODO
-}
type alias Behaviour =
    { jumpAtEnds : Bool
    , handleHomeAndEnd : Bool
    , handlePageDownPageUp : Bool
    , collapsedCount : Count
    , expandedCount : Count
    }


{-| TODO
-}
type Count
    = AnyNumber
    | AtLeast Int
    | AtMost Int
    | Exactly Int



---- VIEW


{-| TODO
-}
view :
    ViewConfig header
    -> (Cmd msg -> Accordion -> msg)
    -> String
    -> Accordion
    -> List (Section header msg)
    -> Html msg
view (ViewConfig behaviour views) lift id (Accordion panelStates) sections =
    let
        noOp =
            lift Cmd.none (Accordion panelStates)

        sectionIds =
            extractSectionIds sections
    in
    Html.dl
        ([ Attributes.id id
         , Attributes.attribute "role" "presentation"
         ]
            |> appendAttributes noOp views.dl
        )
        (sections
            |> List.map (viewSection behaviour views lift id panelStates sectionIds)
            |> List.concat
        )


extractSectionIds : List (Section header msg) -> List String
extractSectionIds =
    List.map (\(Section _ { id }) -> id)


viewSection :
    Behaviour
    -> Views header
    -> (Cmd msg -> Accordion -> msg)
    -> String
    -> Dict String PanelState
    -> List String
    -> Section header msg
    -> List (Html msg)
viewSection behaviour views lift id panelStates sectionIds (Section initialState data) =
    let
        buttonId =
            printHeaderId id data.id

        panelId =
            printPanelId id data.id

        header =
            views.button actualState data.header

        actualState =
            panelStates
                |> Dict.get data.id
                |> Maybe.withDefault initialState

        noOp =
            lift Cmd.none (Accordion panelStates)

        onPageDownPageUp attrs =
            if behaviour.handlePageDownPageUp then
                Events.preventDefaultOn "keydown"
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\code ->
                                case code of
                                    "PageDown" ->
                                        Decode.succeed
                                            ( lift
                                                (focusNextHeader behaviour.jumpAtEnds
                                                    noOp
                                                    sectionIds
                                                    id
                                                    data.id
                                                )
                                                (Accordion panelStates)
                                            , True
                                            )

                                    "PageUp" ->
                                        Decode.succeed
                                            ( lift
                                                (focusPreviousHeader behaviour.jumpAtEnds
                                                    noOp
                                                    sectionIds
                                                    id
                                                    data.id
                                                )
                                                (Accordion panelStates)
                                            , True
                                            )

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                    )
                    :: attrs
            else
                attrs
    in
    [ Html.dt
        ([ Attributes.attribute "role" "heading"
         , Attributes.attribute "aria-level" (String.fromInt 3) -- TODO is this always correct?
         ]
            |> appendAttributes noOp views.dt
        )
        [ Html.button
            ([ Attributes.id buttonId
             , Attributes.attribute "aria-expanded" <|
                case actualState of
                    Collapsed ->
                        "false"

                    Expanded ->
                        "true"
             , Attributes.attribute "aria-controls" panelId
             , Events.onClick
                (panelStates
                    |> Dict.update data.id
                        (\maybePanelState ->
                            case maybePanelState of
                                Nothing ->
                                    case initialState of
                                        Collapsed ->
                                            Just Expanded

                                        Expanded ->
                                            Just Collapsed

                                Just Collapsed ->
                                    Just Expanded

                                Just Expanded ->
                                    Just Collapsed
                        )
                    |> Accordion
                    |> lift Cmd.none
                )
             , Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "ArrowUp" ->
                                    Decode.succeed
                                        ( lift
                                            (focusPreviousHeader behaviour.jumpAtEnds
                                                noOp
                                                sectionIds
                                                id
                                                data.id
                                            )
                                            (Accordion panelStates)
                                        , True
                                        )

                                "ArrowDown" ->
                                    Decode.succeed
                                        ( lift
                                            (focusNextHeader behaviour.jumpAtEnds
                                                noOp
                                                sectionIds
                                                id
                                                data.id
                                            )
                                            (Accordion panelStates)
                                        , True
                                        )

                                "Home" ->
                                    if behaviour.handleHomeAndEnd then
                                        Decode.succeed
                                            ( lift (focusFirstHeader noOp sectionIds id)
                                                (Accordion panelStates)
                                            , True
                                            )
                                    else
                                        Decode.fail "not handling that key here"

                                "End" ->
                                    if behaviour.handleHomeAndEnd then
                                        Decode.succeed
                                            ( lift (focusLastHeader noOp sectionIds id)
                                                (Accordion panelStates)
                                            , True
                                            )
                                    else
                                        Decode.fail "not handling that key here"

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )
             ]
                |> appendAttributes noOp header.attributes
            )
            (List.map (Html.map (\_ -> noOp)) header.children)
        ]
    , Html.dd
        ([ Attributes.attribute "role" "region"
         , Attributes.attribute "aria-labelledby" buttonId
         , Attributes.id panelId
         ]
            |> onPageDownPageUp
            |> applyPanelState actualState
            |> appendAttributes noOp views.dd
        )
        data.panel
    ]


focusPreviousHeader : Bool -> msg -> List String -> String -> String -> Cmd msg
focusPreviousHeader jumpAtEnds noOp sectionIds id currentSectionId =
    focusNextHeader jumpAtEnds noOp (List.reverse sectionIds) id currentSectionId


focusNextHeader : Bool -> msg -> List String -> String -> String -> Cmd msg
focusNextHeader jumpAtEnds noOp sectionIds id currentSectionId =
    case sectionIds of
        [] ->
            Cmd.none

        firstId :: _ ->
            focusNextHeaderHelp firstId jumpAtEnds noOp sectionIds id currentSectionId


focusNextHeaderHelp : String -> Bool -> msg -> List String -> String -> String -> Cmd msg
focusNextHeaderHelp firstId jumpAtEnds noOp sectionIds id currentSectionId =
    let
        focus headerId =
            Dom.focus (printHeaderId id headerId)
                |> Task.attempt (\_ -> noOp)
    in
    case sectionIds of
        [] ->
            Cmd.none

        currentId :: rest ->
            case rest of
                [] ->
                    if currentId == currentSectionId && jumpAtEnds then
                        focus firstId
                    else
                        Cmd.none

                nextId :: _ ->
                    if currentId == currentSectionId then
                        focus nextId
                    else
                        focusNextHeaderHelp firstId jumpAtEnds noOp rest id currentSectionId


focusFirstHeader : msg -> List String -> String -> Cmd msg
focusFirstHeader noOp sectionIds id =
    case List.head sectionIds of
        Nothing ->
            Cmd.none

        Just firstId ->
            Dom.focus (printHeaderId id firstId)
                |> Task.attempt (\_ -> noOp)


focusLastHeader : msg -> List String -> String -> Cmd msg
focusLastHeader noOp sectionIds id =
    focusFirstHeader noOp (List.reverse sectionIds) id


applyPanelState : PanelState -> List (Html.Attribute msg) -> List (Html.Attribute msg)
applyPanelState panelState attrs =
    case panelState of
        Collapsed ->
            Attributes.style "display" "none" :: attrs

        Expanded ->
            attrs



---- VIEW HELPER


appendAttributes :
    msg
    -> List (Html.Attribute Never)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
appendAttributes noOp neverAttrs attrs =
    attrs
        |> List.append
            (neverAttrs
                |> List.map (Attributes.map (\_ -> noOp))
            )



-- IDS


printHeaderId : String -> String -> String
printHeaderId id headerId =
    id ++ "--" ++ headerId ++ "__accordion-header"


printPanelId : String -> String -> String
printPanelId id panelId =
    id ++ "--" ++ panelId ++ "__accordion-panel"
