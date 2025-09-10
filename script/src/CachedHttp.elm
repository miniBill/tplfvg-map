module CachedHttp exposing (getJson, getString)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode exposing (Decoder)
import Pages.Script as Script
import Result.Extra
import Sha256


getString : String -> BackendTask FatalError String
getString url =
    getCached url File.rawFile


getJson : String -> Decoder data -> BackendTask FatalError data
getJson url decoder =
    getCached url (File.jsonFile decoder)


getCached : String -> (String -> BackendTask { error | fatal : FatalError } data) -> BackendTask FatalError data
getCached url inner =
    let
        filename : String
        filename =
            toFilename url
    in
    inner filename
        |> BackendTask.allowFatal
        |> BackendTask.onError
            (\_ ->
                Do.allowFatal
                    (Http.request
                        { url = url
                        , headers = [ ( "X-Requested-With", "XMLHttpRequest" ) ]
                        , method = "GET"
                        , body = Http.emptyBody
                        , retries = Nothing
                        , timeoutInMs = Nothing
                        }
                        Http.expectString
                        |> BackendTask.onError
                            (\e ->
                                case e.recoverable of
                                    Http.BadStatus metadata _ ->
                                        if metadata.statusCode == 404 then
                                            File.rawFile (".cache-old/" ++ Sha256.sha256 url)
                                                |> BackendTask.onError (\_ -> BackendTask.fail e)

                                        else
                                            BackendTask.fail e

                                    _ ->
                                        BackendTask.fail e
                            )
                    )
                <| \raw ->
                Do.do (Script.sleep 200) <| \_ ->
                Do.allowFatal (Script.writeFile { path = filename ++ ".tmp", body = raw }) <| \_ ->
                Do.command "mv" [ filename ++ ".tmp", filename ] <| \_ ->
                BackendTask.allowFatal (inner filename)
            )


toFilename : String -> String
toFilename url =
    ".cache/" ++ Sha256.sha256 url


isCached : String -> BackendTask error Bool
isCached url =
    let
        filename : String
        filename =
            toFilename url
    in
    Glob.literal filename
        |> Glob.expectUniqueMatch
        |> BackendTask.toResult
        |> BackendTask.map Result.Extra.isOk
