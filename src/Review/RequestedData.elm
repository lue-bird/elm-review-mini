module Review.RequestedData exposing (RequestedData(..), combine, combineJust, none)


type RequestedData
    = RequestedData
        { moduleNameLookup : Bool
        , sourceCodeExtractor : Bool
        , ignoredFiles : Bool
        }


none : RequestedData
none =
    RequestedData
        { moduleNameLookup = False
        , sourceCodeExtractor = False
        , ignoredFiles = False
        }


combine : Maybe RequestedData -> Maybe RequestedData -> RequestedData
combine maybeA maybeB =
    case maybeA of
        Nothing ->
            Maybe.withDefault none maybeB

        Just a ->
            case maybeB of
                Just b ->
                    combineJust a b

                Nothing ->
                    a


combineJust : RequestedData -> RequestedData -> RequestedData
combineJust (RequestedData a) (RequestedData b) =
    RequestedData
        { moduleNameLookup = a.moduleNameLookup || b.moduleNameLookup
        , sourceCodeExtractor = a.sourceCodeExtractor || b.sourceCodeExtractor
        , ignoredFiles = a.ignoredFiles || b.ignoredFiles
        }
