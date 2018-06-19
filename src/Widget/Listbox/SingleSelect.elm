module Widget.Listbox.SingleSelect
    exposing
        ( focusEntry
        , focusNextOrFirstEntry
        , focusPreviousOrFirstEntry
        , update
        )

{-|

@docs update

@docs focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry

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

import Widget.Listbox as Listbox exposing (Entry, Listbox, Msg, UpdateConfig)


{-| TODO
-}
update :
    UpdateConfig a
    -> Listbox
    -> List (Entry a divider)
    -> Maybe a
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe a )
update config listbox allEntries selection msg =
    let
        ( newListbox, cmd, newSelection ) =
            Listbox.update config
                listbox
                allEntries
                (case selection of
                    Nothing ->
                        []

                    Just a ->
                        [ a ]
                )
                msg
    in
    ( newListbox
    , cmd
    , case newSelection of
        [] ->
            Nothing

        a :: _ ->
            Just a
    )


{-| TODO
-}
focusEntry : UpdateConfig a -> a -> Maybe a -> Listbox -> ( Listbox, Maybe a )
focusEntry config newEntry selection listbox =
    Tuple.mapSecond listToMaybe <|
        Listbox.focusEntry config newEntry (maybeToList selection) listbox


{-| TODO
-}
focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Maybe a
    -> Listbox
    -> ( Listbox, Maybe a )
focusNextOrFirstEntry config allEntries selection listbox =
    Tuple.mapSecond listToMaybe <|
        Listbox.focusNextOrFirstEntry config
            allEntries
            (maybeToList selection)
            listbox


{-| TODO
-}
focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Maybe a
    -> Listbox
    -> ( Listbox, Maybe a )
focusPreviousOrFirstEntry config allEntries selection listbox =
    Tuple.mapSecond listToMaybe <|
        Listbox.focusPreviousOrFirstEntry config
            allEntries
            (maybeToList selection)
            listbox


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]


listToMaybe : List a -> Maybe a
listToMaybe listA =
    case listA of
        [] ->
            Nothing

        a :: _ ->
            Just a
