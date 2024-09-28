module ElmJson.LocalExtra exposing (sourceDirectories)

import Elm.Project


sourceDirectories : Elm.Project.Project -> List String
sourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs
                |> List.map
                    (\directoryPath ->
                        directoryPath
                            |> removeDotSlashAtBeginning
                            |> pathMakeOSAgnostic
                            |> endWithSlash
                    )

        Elm.Project.Package _ ->
            [ "src/" ]


pathMakeOSAgnostic : String -> String
pathMakeOSAgnostic path =
    path |> String.replace "\\" "/"


removeDotSlashAtBeginning : String -> String
removeDotSlashAtBeginning directoryPath =
    if directoryPath |> String.startsWith "./" then
        String.dropLeft 2 directoryPath

    else
        directoryPath


endWithSlash : String -> String
endWithSlash directoryPath =
    if directoryPath |> String.endsWith "/" then
        directoryPath

    else
        directoryPath ++ "/"
