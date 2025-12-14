module Review.Error.Target exposing
    ( Target(..)
    , elmJson
    , extraFile
    , module_
    , readme
    , setCurrentFilePathOnTargetIfNeeded
    )

import Review.Error.FileTarget as FileTarget exposing (FileTarget)


type Target
    = FileTarget FileTarget
    | Global
    | UserGlobal


module_ : String -> Target
module_ path =
    FileTarget (FileTarget.Module path)


elmJson : Target
elmJson =
    FileTarget FileTarget.ElmJson


readme : Target
readme =
    FileTarget FileTarget.Readme


extraFile : String -> Target
extraFile path =
    FileTarget (FileTarget.ExtraFile path)


setCurrentFilePathOnTargetIfNeeded : String -> Target -> Target
setCurrentFilePathOnTargetIfNeeded path target =
    case target of
        FileTarget fileTarget ->
            FileTarget.setCurrentFilePathOnTargetIfNeeded path fileTarget
                |> FileTarget

        Global ->
            target

        UserGlobal ->
            target
