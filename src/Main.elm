module Main exposing (..)

import Dict exposing (Dict)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc
import Regex
import Dict.Extra


onlyMarkdownFile entry =
    case entry of
        File.File s ->
            if String.endsWith ".md" s then
                Just (String.dropRight 3 s)
            else
                Nothing
        _ -> Nothing


separator =
    "----- Backlinks"


linkRegex =
    Regex.fromString "\\[\\[([^\\]]+)\\]\\]"
        |> Maybe.withDefault Regex.never


relationsToDict relations =
    relations
        |> Dict.Extra.groupBy .link
        |> Dict.map (\k v -> List.map .filename v)


findLinksInContents contents =
    Regex.find linkRegex contents
        |> List.concatMap .submatches
        |> List.filterMap identity


removeBacklinksFromContents contents =
    case String.split ("\n\n" ++ separator) contents of
        [ note, backlinks ] ->
            note

        _ ->
            contents


addBacklinksToContents backlinks contents =
    let
        backlinksContents =
            List.map (\s -> "- [[" ++ s ++ "]]") backlinks
                |> String.join("\n")
    in
    contents ++ "\n\n" ++ separator ++ "\n\n" ++ backlinksContents


program : Process -> IO ()
program process =
    case process.argv of
        [ _, path ] ->
            IO.do (File.readDir path |> IO.exitOnError identity) <| (\entries ->
                let
                    readFiles =
                        entries
                            |> List.filterMap onlyMarkdownFile
                            |> List.map (\filename ->
                                File.contentsOf filename
                                    |> IO.exitOnError identity
                                    |> IO.map (\contents -> { filename = filename, contents = contents })
                            )
                            |> IO.combine
                in
            IO.do readFiles <| \files ->
                let
                    cleanedFiles =
                        files
                            |> List.map (\file -> { file | contents = removeBacklinksFromContents file.contents })

                    relations =
                        cleanedFiles
                            |> List.concatMap (\{ filename, contents } ->
                                List.map (\link -> { link = link, filename = filename }) (findLinksInContents contents)
                            )
                            |> relationsToDict

                    filesWithBacklinks =
                        cleanedFiles
                            |> List.map (\{ filename, contents } ->
                                { filename = filename, contents = contents, backlinks = Maybe.withDefault [] <| Dict.get filename relations }
                            )

                    writeFiles =
                        filesWithBacklinks
                            |> List.map (\f -> { filename = f.filename ++ ".md", contents = addBacklinksToContents f.backlinks f.contents })
                            |> List.map (\{ filename, contents } -> File.writeContentsTo filename contents)
                            |> IO.combine
                in
            IO.do writeFiles <| \_ ->
            IO.return ()
            )

        _ ->
            Proc.logErr ("Usage: elm-cli <program> file\n")
