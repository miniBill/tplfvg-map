module DownloadData exposing (run)

import Angle
import BackendTask exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.Do as Do
import Elm
import Elm.Annotation
import FatalError exposing (FatalError)
import Gen.Angle
import Gen.Id
import Gen.Types
import Id
import Json.Decode
import Json.Encode
import Pages.Script as Script exposing (Script)
import ScriptApi
import SeqSet
import Types


run : Script
run =
    Script.withoutCliOptions
        (Do.allowFatal
            (Custom.run "profileStart"
                Json.Encode.null
                (Json.Decode.succeed ())
            )
         <| \_ ->
         Do.do task <| \_ ->
         Do.allowFatal
             (Custom.run "profileEnd"
                 Json.Encode.null
                 (Json.Decode.succeed ())
             )
         <|
             \_ -> Do.noop
        )


task : BackendTask FatalError ()
task =
    Do.do ScriptApi.getStops <| \stops ->
    Do.log ("Stops: " ++ String.fromInt (List.length stops)) <| \_ ->
    Do.do (ScriptApi.getEndpoints (List.map .code stops {- Id.fromString "70101" -})) <| \endpoints ->
    Do.allowFatal
        (Script.writeFile
            { path = "../generated/Data.elm"
            , body =
                [ stops
                    |> List.map
                        (\stopInfo ->
                            Elm.record
                                [ ( "name", Elm.string stopInfo.name )
                                , ( "code"
                                  , Gen.Id.fromString (Id.toString stopInfo.code)
                                  )
                                , ( "commune", Elm.string stopInfo.commune )
                                , ( "coordinates"
                                  , Elm.record
                                        [ ( "longitude"
                                          , Gen.Angle.degrees (Angle.inDegrees stopInfo.coordinates.longitude)
                                          )
                                        , ( "latitude"
                                          , Gen.Angle.degrees (Angle.inDegrees stopInfo.coordinates.latitude)
                                          )
                                        ]
                                  )
                                , ( "services"
                                  , stopInfo.services
                                        |> List.map
                                            (\service ->
                                                case service of
                                                    Types.ExtraUrban ->
                                                        Gen.Types.make_.extraUrban

                                                    Types.Urban ->
                                                        Gen.Types.make_.urban

                                                    Types.Maritime ->
                                                        Gen.Types.make_.maritime
                                            )
                                        |> Elm.list
                                  )
                                ]
                                |> Elm.withType Gen.Types.annotation_.stopInfo
                        )
                    |> Elm.list
                    |> Elm.declaration "stops"
                    |> Elm.expose
                , endpoints
                    |> SeqSet.toList
                    |> List.map (\id -> Gen.Id.fromString (Id.toString id))
                    |> Elm.list
                    |> Elm.withType (Elm.Annotation.list (Gen.Id.annotation_.id Gen.Id.annotation_.stop))
                    |> Elm.declaration "endpoints"
                    |> Elm.expose
                ]
                    |> Elm.file [ "Data" ]
                    |> .contents
            }
        )
    <| \_ ->
    Do.exec "elm-format" [ "../generated" ] <| \_ ->
    BackendTask.succeed ()
