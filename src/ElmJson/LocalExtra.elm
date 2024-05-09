module ElmJson.LocalExtra exposing (sourceDirectories)

import Elm.Project


sourceDirectories : Elm.Project.Project -> List String
sourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs |> List.map (\dir -> dir |> removeDotSlashAtBeginning |> pathMakeOSAgnostic |> endWithSlash)

        Elm.Project.Package _ ->
            [ "src/" ]


pathMakeOSAgnostic : String -> String
pathMakeOSAgnostic =
    \path -> path |> String.replace "\\" "/"


removeDotSlashAtBeginning : String -> String
removeDotSlashAtBeginning dir =
    if String.startsWith "./" dir then
        String.dropLeft 2 dir

    else
        dir


endWithSlash : String -> String
endWithSlash dir =
    if String.endsWith "/" dir then
        dir

    else
        dir ++ "/"
