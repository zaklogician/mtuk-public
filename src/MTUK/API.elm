module MTUK.API exposing (..)

import Task exposing (Task, succeed, perform)
import Http exposing (..)
import Json.Decode as Decode exposing (field, string, int, list)
import Json.Encode as Encode
import Sha256 exposing (sha256)


type alias Content =
  { key : String
  , value : String
  }

type Msg = ReadSuccess Content
         | ReadFailure String
         | UpdateSuccess Content
         | UpdateFailure String

testServerURL : String
testServerURL = "https://testserver.url/api/"

readURL : String
readURL = testServerURL ++ "read"

updateURL : String
updateURL = testServerURL ++ "update"
--------------------------------------------------------------------------------
-- Read 

decodeContent : Decode.Decoder Content
decodeContent =
  Decode.map2 Content
      (field "key" string)
      (field "value" string)


reqRead : String -> Request Content
reqRead key =
    request
        { method = "GET"
        , headers = []
        , url = readURL ++ "?key=" ++ key
        , body = Http.emptyBody
        , expect = expectJson decodeContent
        , timeout = Nothing
        , withCredentials = False
        }

failRead : Result Http.Error Content -> Msg
failRead result =
    case result of
        Ok content -> ReadSuccess content
        Err err -> ReadFailure <| case err of
          BadUrl url -> "BadUrl " ++ url
          Timeout -> "Timeout"
          NetworkError -> "NetworkError"
          BadStatus status -> "BadStatus " ++ toString status
          BadPayload payload status -> "BadPayload " ++ toString payload

read : String -> Cmd Msg
read key = Http.send failRead (reqRead key)


-- TODO: There is a proper Elm JWT library now. Consider replacing this.
type alias SignedContent =
    { key : String
    , value : String
    , signature : String
    }

sign : String -> Content -> SignedContent
sign passwd content =
    let
        payload =
            sha256 passwd      ++ 
            sha256 content.key ++
            sha256 content.value
        signature_ = sha256 payload
    in
        { key = content.key
        , value = content.value
        , signature = signature_
        }

encodeSignedContent : SignedContent -> String
encodeSignedContent signed =
    Encode.encode 0 <| Encode.object
        [ ("key", Encode.string signed.key)
        , ("value", Encode.string signed.value)
        , ("signature", Encode.string signed.signature)
        ]

--------------------------------------------------------------------------------
-- Update

reqUpdate : String -> Content -> Request Content
reqUpdate passwd content =
    let
        signed = sign passwd content
    in
        request
            { method = "POST"
            , headers = []
            , url = updateURL
            , body = stringBody "application/json" (encodeSignedContent signed)
            , expect = expectJson decodeContent
            , timeout = Nothing
            , withCredentials = False
            }

failUpdate : Result Http.Error Content -> Msg
failUpdate result =
    case result of
        Ok content -> UpdateSuccess content
        Err err -> UpdateFailure <| case err of
          BadUrl url -> "BadUrl " ++ url
          Timeout -> "Timeout"
          NetworkError -> "NetworkError"
          BadStatus status -> "BadStatus " ++ toString status
          BadPayload payload status -> "BadPayload " ++ toString payload ++ ":::" ++ toString status

update : String -> Content -> Cmd Msg
update passwd content = Http.send failUpdate (reqUpdate passwd content)
