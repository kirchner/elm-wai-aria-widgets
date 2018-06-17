module Widget.Dialog
    exposing
        ( Dialog
        , ViewConfig
        , Views
        , close
        , init
        , open
        , view
        , viewConfig
        , wrapToEnd
        , wrapToStart
        )

{-|

@docs Dialog, init, open, close, view


# Configuration

@docs ViewConfig, viewConfig, Views


# Focus trap

@docs wrapToStart, wrapToEnd

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

import Browser exposing (DomError)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Task exposing (Task)
import Widget exposing (HtmlAttributes)


{-| TODO
-}
type Dialog
    = Dialog DialogData


type alias DialogData =
    { open : Bool }


{-| TODO
-}
init : Dialog
init =
    Dialog { open = False }


{-| TODO
-}
open : (Cmd msg -> msg) -> String -> ( Dialog, Cmd msg )
open lift firstFocusId =
    ( Dialog { open = True }
    , Task.attempt (\_ -> lift Cmd.none) <|
        Browser.focus firstFocusId
    )


{-| TODO
-}
close : (Cmd msg -> msg) -> String -> ( Dialog, Cmd msg )
close lift firstFocusId =
    ( Dialog { open = False }
    , Task.attempt (\_ -> lift Cmd.none) <|
        Browser.focus firstFocusId
    )



---- UPDATE


{-| TODO
-}
wrapToStart : (Cmd msg -> msg) -> String -> Html.Attribute msg
wrapToStart lift lastFocusId =
    Events.preventDefaultOn "keydown"
        (Decode.map2 Tuple.pair
            (Decode.field "key" Decode.string)
            (Decode.field "shiftKey" Decode.bool)
            |> Decode.andThen
                (\keyInfo ->
                    case keyInfo of
                        ( "Tab", False ) ->
                            Decode.succeed
                                ( lift <|
                                    Task.attempt (\_ -> lift Cmd.none) <|
                                        Browser.focus lastFocusId
                                , True
                                )

                        _ ->
                            Decode.fail "not handling that key here"
                )
        )


{-| TODO
-}
wrapToEnd : (Cmd msg -> msg) -> String -> Html.Attribute msg
wrapToEnd lift firstFocusId =
    Events.preventDefaultOn "keydown"
        (Decode.map2 Tuple.pair
            (Decode.field "key" Decode.string)
            (Decode.field "shiftKey" Decode.bool)
            |> Decode.andThen
                (\keyInfo ->
                    case keyInfo of
                        ( "Tab", True ) ->
                            Decode.succeed
                                ( lift <|
                                    Task.attempt (\_ -> lift Cmd.none) <|
                                        Browser.focus firstFocusId
                                , True
                                )

                        _ ->
                            Decode.fail "not handling that key here"
                )
        )



---- CONFIG


{-| TODO
-}
type ViewConfig msg
    = ViewConfig (Views msg)


{-| TODO
-}
viewConfig : Views msg -> ViewConfig msg
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views msg =
    { container : Bool -> List (Html.Attribute msg)
    , backdrop : List (Html.Attribute msg)
    }



---- VIEW


{-| TODO
-}
view :
    ViewConfig msg
    -> msg
    -> Dialog
    -> List (Html msg)
    -> Html msg
view (ViewConfig views) backdropClicked (Dialog data) content =
    Html.div
        (views.container data.open)
        (Html.div (Events.onClick backdropClicked :: views.backdrop) []
            :: content
        )
