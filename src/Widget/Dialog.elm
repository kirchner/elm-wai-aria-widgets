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
        )

{-|

@docs Dialog

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

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Widget exposing (HtmlAttributes)


type Dialog
    = Dialog DialogData


type alias DialogData =
    { open : Bool }


init : Dialog
init =
    Dialog { open = False }


open : Dialog -> Dialog
open (Dialog data) =
    Dialog { data | open = True }


close : Dialog -> Dialog
close (Dialog data) =
    Dialog { data | open = False }



---- CONFIG


type ViewConfig msg
    = ViewConfig (Views msg)


viewConfig : Views msg -> ViewConfig msg
viewConfig =
    ViewConfig


type alias Views msg =
    { container : Bool -> List (Html.Attribute msg)
    , backdrop : List (Html.Attribute msg)
    }


view : ViewConfig msg -> Dialog -> msg -> List (Html msg) -> Html msg
view (ViewConfig views) (Dialog data) closed content =
    Html.div
        (views.container data.open)
        (Html.div
            (Events.onClick closed :: views.backdrop)
            []
            :: content
        )
