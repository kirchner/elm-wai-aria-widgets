module Widget.Accordion
    exposing
        ( Accordion
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

    TODO

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
    = ViewConfig (Views header)


{-| TODO
-}
viewConfig : Views header -> ViewConfig header
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
view (ViewConfig views) lift id (Accordion panelStates) sections =
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
            |> List.map (viewSection views lift id panelStates sectionIds)
            |> List.concat
        )


extractSectionIds : List (Section header msg) -> List String
extractSectionIds =
    List.map (\(Section _ { id }) -> id)


viewSection :
    Views header
    -> (Cmd msg -> Accordion -> msg)
    -> String
    -> Dict String PanelState
    -> List String
    -> Section header msg
    -> List (Html msg)
viewSection views lift id panelStates sectionIds (Section initialState data) =
    let
        buttonId =
            id ++ "--" ++ data.id ++ "__accordion-header"

        panelId =
            id ++ "--" ++ data.id ++ "__accordion-panel"

        header =
            views.button actualState data.header

        actualState =
            panelStates
                |> Dict.get data.id
                |> Maybe.withDefault initialState

        noOp =
            lift Cmd.none (Accordion panelStates)
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
             , Events.preventDefaultOn "keypress"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "ArrowUp" ->
                                    Decode.succeed
                                        ( lift (focusPreviousHeader noOp sectionIds id data.id)
                                            (Accordion panelStates)
                                        , True
                                        )

                                "ArrowDown" ->
                                    Decode.succeed
                                        ( lift (focusNextHeader noOp sectionIds id data.id)
                                            (Accordion panelStates)
                                        , True
                                        )

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
            |> applyPanelState actualState
            |> appendAttributes noOp views.dd
        )
        data.panel
    ]


focusPreviousHeader : msg -> List String -> String -> String -> Cmd msg
focusPreviousHeader noOp sectionIds id currentSectionId =
    focusNextHeader noOp (List.reverse sectionIds) id currentSectionId


focusNextHeader : msg -> List String -> String -> String -> Cmd msg
focusNextHeader noOp sectionIds id currentSectionId =
    case sectionIds of
        [] ->
            Cmd.none

        currentId :: [] ->
            Cmd.none

        currentId :: nextId :: rest ->
            if currentId == currentSectionId then
                Browser.focus (id ++ "--" ++ nextId ++ "__accordion-header")
                    |> Task.attempt (\_ -> noOp)
            else
                focusNextHeader noOp (nextId :: rest) id currentSectionId


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
    neverAttrs
        |> List.map (Attributes.map (\_ -> noOp))
        |> List.append attrs
