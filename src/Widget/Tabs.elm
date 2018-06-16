module Widget.Tabs
    exposing
        ( Tab
        , Tabs
        , ViewConfig
        , Views
        , init
        , tab
        , view
        , viewConfig
        )

{-|

@docs Tabs, init, view, tab, Tab


# Configuration

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

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import List.Extra as List
import Task
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| TODO
-}
type Tabs
    = Tabs String


{-| TODO
-}
init : String -> Tabs
init tabId =
    Tabs tabId


{-| TODO
-}
type Tab tab msg
    = Tab (TabData tab msg)


type alias TabData tab msg =
    { id : String
    , tab : tab
    , tabpanel : List (Html msg)
    , focusable : Bool
    }


{-| TODO
-}
tab :
    { id : String
    , tab : tab
    , tabpanel : List (Html msg)
    , focusable : Bool
    }
    -> Tab tab msg
tab =
    Tab



---- CONFIG


{-| TODO
-}
type ViewConfig tab
    = ViewConfig (Views tab)


{-| TODO
-}
viewConfig : Views tab -> ViewConfig tab
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views tab =
    { tabs : HtmlAttributes
    , tablist : HtmlAttributes
    , button : Bool -> tab -> HtmlDetails
    , tabpanel : HtmlAttributes
    }



---- VIEW


{-| TODO
-}
view :
    ViewConfig tab
    ->
        { id : String
        , label : String
        , lift : Cmd msg -> Tabs -> msg
        }
    -> Tabs
    -> List (Tab tab msg)
    -> Html msg
view (ViewConfig views) { id, label, lift } (Tabs openTabId) tabs =
    let
        noOp =
            lift Cmd.none (Tabs openTabId)

        viewTabpanel (Tab data) =
            Html.div
                ([ Attributes.id (id ++ "-" ++ data.id ++ "-tabpanel")
                 , Attributes.attribute "role" "tabpanel"
                 , Attributes.attribute "aria-labelledby" <|
                    id
                        ++ "-"
                        ++ data.id
                        ++ "-tab"
                 ]
                    |> setTabindex data.focusable
                    |> setHidden (data.id == openTabId)
                    |> appendAttributes noOp views.tabpanel
                )
                data.tabpanel

        setTabindex focusable attrs =
            if focusable then
                Attributes.tabindex 0 :: attrs
            else
                attrs

        setHidden open attrs =
            if open then
                attrs
            else
                Attributes.hidden True :: attrs
    in
    Html.div
        (appendAttributes noOp views.tabs [])
        (viewTablist views id label lift openTabId tabs
            :: List.map viewTabpanel tabs
        )


viewTablist :
    Views tab
    -> String
    -> String
    -> (Cmd msg -> Tabs -> msg)
    -> String
    -> List (Tab tab msg)
    -> Html msg
viewTablist views id label lift openTabId tabs =
    let
        noOp =
            lift Cmd.none (Tabs openTabId)

        openTab tabId =
            lift
                (Browser.focus (id ++ "-" ++ tabId ++ "-tab")
                    |> Task.attempt (\_ -> lift Cmd.none (Tabs tabId))
                )
                (Tabs tabId)

        openPreviousTab =
            tabIds
                |> List.splitWhen (\thisId -> thisId == openTabId)
                |> Maybe.andThen (\( start, end ) -> List.head (List.reverse start))
                |> Maybe.map openTab
                |> Maybe.withDefault noOp

        openNextTab =
            tabIds
                |> List.splitWhen (\thisId -> thisId == openTabId)
                |> Maybe.andThen (\( start, end ) -> List.head (List.drop 1 end))
                |> Maybe.map openTab
                |> Maybe.withDefault noOp

        openFirstTab =
            tabIds
                |> List.head
                |> Maybe.map openTab
                |> Maybe.withDefault noOp

        openLastTab =
            tabIds
                |> List.reverse
                |> List.head
                |> Maybe.map openTab
                |> Maybe.withDefault noOp

        tabIds =
            List.map (\(Tab data) -> data.id) tabs

        setTabindex open attrs =
            if open then
                attrs
            else
                Attributes.tabindex -1 :: attrs

        viewTabButton (Tab data) =
            let
                { attributes, children } =
                    views.button open data.tab

                open =
                    data.id == openTabId
            in
            Html.button
                ([ Attributes.id (id ++ "-" ++ data.id ++ "-tab")
                 , Attributes.attribute "role" "tab"
                 , Attributes.attribute "aria-selected" (boolToString open)
                 , Attributes.attribute "aria-controls" <|
                    id
                        ++ "-"
                        ++ data.id
                        ++ "-tabpanel"
                 , Events.onClick (lift Cmd.none (Tabs data.id))
                 , Events.preventDefaultOn "keypress"
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\code ->
                                case code of
                                    "ArrowLeft" ->
                                        Decode.succeed ( openPreviousTab, True )

                                    "ArrowRight" ->
                                        Decode.succeed ( openNextTab, True )

                                    "Home" ->
                                        Decode.succeed ( openFirstTab, True )

                                    "End" ->
                                        Decode.succeed ( openLastTab, True )

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                    )
                 ]
                    |> setTabindex open
                    |> appendAttributes noOp attributes
                )
                (List.map (Html.map (\_ -> noOp)) children)
    in
    Html.div
        ([ Attributes.attribute "role" "tablist"
         , Attributes.attribute "aria-label" label
         ]
            |> appendAttributes noOp views.tablist
        )
        (List.map viewTabButton tabs)



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


boolToString : Bool -> String
boolToString b =
    if b then
        "true"
    else
        "false"
