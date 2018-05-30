module Internal.Entries
    exposing
        ( Next(..)
        , Previous(..)
        , find
        , findNext
        , findPrevious
        , findWith
        , indexOf
        )


indexOf : (a -> String) -> String -> List a -> Maybe Int
indexOf =
    indexOfHelp 0


indexOfHelp : Int -> (a -> String) -> String -> List a -> Maybe Int
indexOfHelp index uniqueId id entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == id then
                Just index
            else
                indexOfHelp (index + 1) uniqueId id rest


find : (a -> String) -> String -> List a -> Maybe ( Int, a )
find =
    findHelp 0


findHelp : Int -> (a -> String) -> String -> List a -> Maybe ( Int, a )
findHelp index entryId selectedId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if entryId entry == selectedId then
                Just ( index, entry )
            else
                findHelp (index + 1) entryId selectedId rest


findWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
findWith matchesQuery uniqueId focus query entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == focus then
                let
                    id =
                        uniqueId entry
                in
                if matchesQuery query entry then
                    Just id
                else
                    proceedWith matchesQuery uniqueId id query rest
            else
                findWith matchesQuery uniqueId focus query rest


proceedWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
proceedWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Just id

        next :: rest ->
            if matchesQuery query next then
                Just (uniqueId next)
            else
                proceedWith matchesQuery uniqueId id query rest


type Previous a
    = Previous Int a
    | Last a


findPrevious : (a -> String) -> String -> List a -> Maybe (Previous a)
findPrevious entryId currentId entries =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if entryId first == currentId then
                entries
                    |> List.reverse
                    |> List.head
                    |> Maybe.map Last
            else
                findPreviousHelp first 0 entryId currentId rest


findPreviousHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Previous a)
findPreviousHelp previous index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        next :: rest ->
            if entryId next == currentId then
                Just (Previous index previous)
            else
                findPreviousHelp next (index + 1) entryId currentId rest


type Next a
    = Next Int a
    | First a


findNext : (a -> String) -> String -> List a -> Maybe (Next a)
findNext entryId currentId entries =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            case rest of
                [] ->
                    if entryId first == currentId then
                        Just (First first)
                    else
                        Nothing

                next :: _ ->
                    if entryId first == currentId then
                        Just (Next 1 next)
                    else
                        findNextHelp first 1 entryId currentId rest


findNextHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Next a)
findNextHelp first index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            case rest of
                [] ->
                    Just (First first)

                next :: _ ->
                    if entryId entry == currentId then
                        Just (Next (index + 1) next)
                    else
                        findNextHelp first (index + 1) entryId currentId rest
