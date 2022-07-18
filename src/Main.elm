port module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Http
import Json.Decode as Decode exposing (Decoder, Error(..), Value, decodeValue, field, int, string)
import Json.Encode as Encode
import Process
import Task
import Time exposing (Posix)
import Validate exposing (Validator)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Some constants


supabaseURL : String
supabaseURL =
    "https://bgwgivatowayfodanvqf.supabase.co"


supabaseAnonymousToken : String
supabaseAnonymousToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJnd2dpdmF0b3dheWZvZGFudnFmIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NTY2NTkxNDEsImV4cCI6MTk3MjIzNTE0MX0.AfP9p5wZsXZkSbXwcTGAMwELeB1HtX1Q0iiAvWr5Glw"


type alias Model =
    { --  General stuff
      supabaseAuthorisedToken : String
    , screenToShow : Screen

    -- Login and Sign up stuff
    , email : String
    , password : String
    , loginErrors : Errors
    , signUpErrors : Errors
    , currentUsersId : Maybe String

    -- User's data
    , uploadedFiles : List UploadedFile

    -- Files being uploaded
    , lastUsedIndexNumberForFilesBeingUploaded : Int
    , filesBeingUploaded : FilesBeingUploaded

    -- Files being read
    , lastUsedIndexNumberForFilesBeingRead : Int
    , filesBeingRead : FilesBeingRead

    --  Reports display stuff
    , reports : Reports
    , searchTerm : String
    }


type Screen
    = Login
    | SignUp
    | Display


type alias SessionDetails =
    { token : String
    , userId : String
    }


type alias Errors =
    List String


type alias FileBeingUploaded =
    { id : Int
    , name : String
    , size : Int
    , mime : String
    , value : Value
    , uploadStatus : UploadStatus
    }


type UploadStatus
    = Uploading Float
    | Uploaded
    | ErrorWhileUploading String


type alias FilesBeingUploaded =
    List FileBeingUploaded


type alias FilesBeingRead =
    List FileBeingRead


type alias FileBeingRead =
    { id : Int
    , name : String
    , size : Int
    , mime : String
    , value : Value
    , readStatus : ReadStatus
    }


type ReadStatus
    = WaitingToBeRead
    | Reading Float
    | Read String
    | ErrorWhileReading String


type alias Report =
    { id : String
    , name : String
    , size : Int
    , mime : String
    , uploadedOn : Posix
    }


type alias Reports =
    List Report


type alias UploadedFile =
    { id : Int
    , name : String
    , size : Int
    , mime : String
    , createdAt : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { supabaseAuthorisedToken = ""
      , screenToShow = Login
      , email = "azurewaters@gmail.com"
      , password = "T3$t(er)"
      , loginErrors = []
      , signUpErrors = []
      , currentUsersId = Nothing
      , uploadedFiles = []
      , reports = []
      , lastUsedIndexNumberForFilesBeingUploaded = 0
      , filesBeingUploaded = []
      , lastUsedIndexNumberForFilesBeingRead = 0
      , filesBeingRead = []
      , searchTerm = ""
      }
    , Cmd.none
    )


type Msg
    = --  Login and registration messages
      EmailTyped String
    | PasswordTyped String
    | LoginClicked
    | SignUpOnLoginClicked
    | SignUpClicked
    | LoginOnSignUpClicked
    | UserSignedIn (Result Http.Error String)
    | FetchedUploadFiles (Result Http.Error (List UploadedFile))
      --  File upload messages
    | FilesDropped (List Value)
    | FileUploadProgressed Int Float
    | FileUploadErrored Int
    | FileUploadCompleted Int
    | UnlistUploadedFile Int
      -- File read messages
    | FileTextRead Int String
      --  Reports messages
    | ReportAdded Report
    | ReportModified Report
    | ReportRemoved String
      --  Miscellaneous
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailTyped email ->
            ( { model | email = email }, Cmd.none )

        PasswordTyped password ->
            ( { model | password = password }, Cmd.none )

        LoginClicked ->
            case Validate.validate loginValidator model of
                Ok _ ->
                    ( { model | loginErrors = [] }, signInAUser model.email model.password )

                Err errors ->
                    ( { model | loginErrors = errors }, Cmd.none )

        SignUpOnLoginClicked ->
            ( { model | screenToShow = SignUp }, Cmd.none )

        SignUpClicked ->
            case Validate.validate signUpValidator model of
                Ok _ ->
                    ( { model | signUpErrors = [] }, signUpAUser { email = model.email, password = model.password } )

                Err errors ->
                    ( { model | signUpErrors = errors }, Cmd.none )

        LoginOnSignUpClicked ->
            ( { model | screenToShow = Login }, Cmd.none )

        UserSignedIn response ->
            case response of
                Ok accessToken ->
                    ( { model
                        | supabaseAuthorisedToken = accessToken
                        , screenToShow = Display
                      }
                    , fetchTheUsersUploads model.supabaseAuthorisedToken
                    )

                Err _ ->
                    ( { model | loginErrors = "Unable to log in" :: model.loginErrors }, Cmd.none )

        FetchedUploadFiles value ->
            case value of
                Ok result ->
                    ( { model | uploadedFiles = result }, Cmd.none )

                Err err ->
                    ( Debug.log (Debug.toString err) model, Cmd.none )

        FilesDropped values ->
            let
                imageFileValues : List ( File, Value )
                imageFileValues =
                    List.filterMap
                        (\value ->
                            case decodeValue File.decoder value of
                                Ok file ->
                                    if List.member (File.mime file) [ "image/jpeg", "image/png", "application/pdf" ] == True then
                                        Just ( file, value )

                                    else
                                        Nothing

                                Err _ ->
                                    Nothing
                        )
                        values

                namesOfFilesAlreadyBeingUploaded : List String
                namesOfFilesAlreadyBeingUploaded =
                    List.map .name model.filesBeingUploaded

                uniqueImageFileValues : List ( File, Value )
                uniqueImageFileValues =
                    List.filter
                        (\( file, _ ) ->
                            List.member (File.name file) namesOfFilesAlreadyBeingUploaded
                                |> not
                        )
                        imageFileValues

                idsToUse : List Int
                idsToUse =
                    List.range model.lastUsedIndexNumberForFilesBeingUploaded (model.lastUsedIndexNumberForFilesBeingUploaded + List.length uniqueImageFileValues)

                newFilesBeingUploaded : FilesBeingUploaded
                newFilesBeingUploaded =
                    List.map2
                        (\id ( file, value ) ->
                            { id = id
                            , name = File.name file
                            , size = File.size file
                            , mime = File.mime file
                            , value = value
                            , uploadStatus = Uploading 0
                            }
                        )
                        idsToUse
                        uniqueImageFileValues

                commandsToUploadTheFiles : List (Cmd Msg)
                commandsToUploadTheFiles =
                    List.map
                        (\fileBeingUploaded ->
                            uploadAFile { id = fileBeingUploaded.id, file = fileBeingUploaded.value }
                        )
                        newFilesBeingUploaded
            in
            ( { model
                | filesBeingUploaded = List.append model.filesBeingUploaded newFilesBeingUploaded
                , lastUsedIndexNumberForFilesBeingUploaded = model.lastUsedIndexNumberForFilesBeingUploaded + List.length uniqueImageFileValues
              }
            , Cmd.batch commandsToUploadTheFiles
            )

        FileUploadProgressed id progress ->
            ( { model
                | filesBeingUploaded =
                    updateFilesBeingUploaded
                        model.filesBeingUploaded
                        id
                        (\fileBeingUploaded -> { fileBeingUploaded | uploadStatus = Uploading progress })
              }
            , Cmd.none
            )

        FileUploadErrored id ->
            ( { model
                | filesBeingUploaded =
                    updateFilesBeingUploaded
                        model.filesBeingUploaded
                        id
                        (\fileBeingUploaded -> { fileBeingUploaded | uploadStatus = ErrorWhileUploading "Error" })
              }
            , Cmd.none
            )

        FileUploadCompleted id ->
            ( { model
                | filesBeingUploaded =
                    updateFilesBeingUploaded
                        model.filesBeingUploaded
                        id
                        (\fileBeingUploaded -> { fileBeingUploaded | uploadStatus = Uploaded })
              }
            , Task.perform (\_ -> UnlistUploadedFile id) (Process.sleep 3000)
            )

        UnlistUploadedFile id ->
            ( { model | filesBeingUploaded = List.filter (\fileBeingUploaded -> fileBeingUploaded.id /= id) model.filesBeingUploaded }, Cmd.none )

        FileTextRead id textInFile ->
            ( { model
                | filesBeingRead =
                    List.map
                        (\fileBeingRead ->
                            if id == fileBeingRead.id then
                                { fileBeingRead | readStatus = Read textInFile }

                            else
                                fileBeingRead
                        )
                        model.filesBeingRead
              }
            , Cmd.none
            )

        ReportAdded newReport ->
            ( { model | reports = List.append model.reports [ newReport ] }, Cmd.none )

        ReportModified modifiedReport ->
            ( { model
                | reports =
                    List.map
                        (\report ->
                            if report.id == modifiedReport.id then
                                modifiedReport

                            else
                                report
                        )
                        model.reports
              }
            , Cmd.none
            )

        ReportRemoved id ->
            ( { model | reports = List.filter (\report -> report.id /= id) model.reports }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateFilesBeingUploaded : FilesBeingUploaded -> Int -> (FileBeingUploaded -> FileBeingUploaded) -> FilesBeingUploaded
updateFilesBeingUploaded filesBeingUploaded id updateFunction =
    List.map
        (\fileBeingUploaded ->
            if fileBeingUploaded.id == id then
                updateFunction fileBeingUploaded

            else
                fileBeingUploaded
        )
        filesBeingUploaded


loginValidator : Validator String Model
loginValidator =
    Validate.firstError
        [ Validate.ifBlank .email "Please type in your email"
        , Validate.ifBlank .password "Please type in your password"
        , Validate.ifInvalidEmail .email (\_ -> "Please enter a valid email")
        ]


signUpValidator : Validator String Model
signUpValidator =
    Validate.firstError
        [ Validate.ifBlank .email "Please type in your email"
        , Validate.ifBlank .password "Please type in your name"
        , Validate.ifInvalidEmail .email (\_ -> "Please enter a valid email")
        ]



-- Ports
-- Port Outs


signInAUser : String -> String -> Cmd Msg
signInAUser email password =
    Http.request
        { method = "POST"
        , url = supabaseURL ++ "/auth/v1/token?grant_type=password"
        , headers =
            [ Http.header "Content-Type" "application/json"
            , Http.header "apikey" supabaseAnonymousToken
            ]
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
                )
        , expect = Http.expectJson UserSignedIn accessTokenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


accessTokenDecoder : Decode.Decoder String
accessTokenDecoder =
    field "access_token" string


fetchTheUsersUploads : String -> Cmd Msg
fetchTheUsersUploads authorisedToken =
    Http.request
        { method = "GET"
        , url = supabaseURL ++ "/rest/v1/UploadedFile?select=id,name,size,mime,createdAt"
        , headers =
            [ Http.header "apiKey" supabaseAnonymousToken
            , Http.header "Authorization" ("Bearer " ++ authorisedToken)
            ]
        , body = Http.emptyBody
        , expect = Http.expectJson FetchedUploadFiles uploadedFilesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


uploadedFilesDecoder : Decode.Decoder (List UploadedFile)
uploadedFilesDecoder =
    Decode.list uploadedFileDecoder


uploadedFileDecoder : Decode.Decoder UploadedFile
uploadedFileDecoder =
    Decode.map5 UploadedFile
        (field "id" int)
        (field "name" string)
        (field "size" int)
        (field "mime" string)
        (field "createdAt" string)



-- port signInAUser :
--     { email : String, password : String }
--     -> Cmd msg --  Since we are not expecting any message in return, the return type is the lowercase 'msg'


port signUpAUser : { email : String, password : String } -> Cmd msg


port uploadAFile : { id : Int, file : Value } -> Cmd msg



-- Port Ins


port userSignedIn : (String -> msg) -> Sub msg


port fileUploadProgressed : (Value -> msg) -> Sub msg


port fileUploadErrored : (Int -> msg) -> Sub msg


port fileUploadCompleted : (Int -> msg) -> Sub msg


port reportAdded : (Value -> msg) -> Sub msg


port reportModified : (Value -> msg) -> Sub msg


port reportRemoved : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screenToShow of
        Display ->
            Sub.batch
                [ reportAdded decodeReportAdded
                , reportModified decodeReportModified
                , reportRemoved decodeReportRemoved
                , fileUploadProgressed decodeFileUploadProgress
                , fileUploadErrored FileUploadErrored
                , fileUploadCompleted FileUploadCompleted
                ]

        _ ->
            Sub.none


decodeReportAdded : Value -> Msg
decodeReportAdded value =
    case decodeValue reportDecoder value of
        Ok report ->
            ReportAdded report

        Err error ->
            Debug.log ("Error while decoding report:" ++ Debug.toString error ++ Debug.toString value) NoOp


decodeReportModified : Value -> Msg
decodeReportModified value =
    case decodeValue reportDecoder value of
        Ok report ->
            ReportModified report

        Err error ->
            Debug.log ("Error while decoding report:" ++ Debug.toString error ++ Debug.toString value) NoOp


decodeReportRemoved : Value -> Msg
decodeReportRemoved value =
    case decodeValue string value of
        Ok id ->
            ReportRemoved id

        Err error ->
            Debug.log ("Error while decoding report id:" ++ Debug.toString error ++ Debug.toString value) NoOp


decodeFileUploadProgress : Value -> Msg
decodeFileUploadProgress value =
    case decodeValue fileUploadProgressDecoder value of
        Ok ( id, progress ) ->
            FileUploadProgressed id progress

        Err _ ->
            NoOp


fileUploadProgressDecoder : Decoder ( Int, Float )
fileUploadProgressDecoder =
    Decode.map2 Tuple.pair (field "id" Decode.int) (field "progress" Decode.float)


reportsDecoder : Decoder Reports
reportsDecoder =
    Decode.list reportDecoder


reportDecoder : Decoder Report
reportDecoder =
    Decode.map5 Report
        (field "id" string)
        (field "name" string)
        (field "size" Decode.int)
        (field "mime" string)
        (field "uploadedOn" Decode.int |> Decode.map Time.millisToPosix)



--  VIEWS


view : Model -> Html Msg
view model =
    case model.screenToShow of
        Login ->
            login model

        SignUp ->
            signUp model

        Display ->
            display model


login : Model -> Html Msg
login model =
    div []
        [ input [ type_ "email", required True, placeholder "Your email address", class "input input-bordered", onInput EmailTyped, value model.email ] []
        , input [ type_ "password", required True, placeholder "Your password", class "input input-bordered", onInput PasswordTyped, value model.password ] []
        , button [ onClick SignUpOnLoginClicked, class "btn" ] [ text "Sign Up" ]
        , button [ onClick LoginClicked, class "btn btn-primary" ] [ text "Login" ]
        , div [] (List.map (\error -> div [] [ text error ]) model.loginErrors)
        ]


signUp : Model -> Html Msg
signUp model =
    div []
        [ input [ type_ "email", required True, placeholder "Your email address", class "input", onInput EmailTyped, value model.email ] []
        , input [ type_ "password", required True, placeholder "Set a password", class "input", onInput PasswordTyped, value model.password ] []
        , button [ onClick LoginOnSignUpClicked, class "btn" ] [ text "Login" ]
        , button [ onClick SignUpClicked, class "btn btn-primary" ] [ text "Sign up" ]
        , div [] (List.map (\error -> div [] [ text error ]) model.signUpErrors)
        ]


display : Model -> Html Msg
display model =
    div
        []
        [ filesDropZone model.filesBeingUploaded model.filesBeingRead
        , uploadedFilesDisplay model.uploadedFiles
        , reportsDisplay model.reports
        ]


filesDropZone : FilesBeingUploaded -> FilesBeingRead -> Html Msg
filesDropZone filesBeingUploaded filesBeingRead =
    div
        [ onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        (List.concat
            [ [ div [] [ text "Drag a report here to upload" ] ]
            , List.map fileDisplay filesBeingUploaded

            -- , [ keyedFileUploaders flags filesBeingUploaded ]
            -- , [ keyedTextGetters filesBeingRead ]
            ]
        )


fileDisplay : FileBeingUploaded -> Html Msg
fileDisplay fileToProcess =
    div
        []
        [ fileToProcess.id |> String.fromInt |> text
        , text " - "
        , getUploadStatus fileToProcess |> text
        ]


getUploadStatus : FileBeingUploaded -> String
getUploadStatus fileToProcess =
    case fileToProcess.uploadStatus of
        Uploading progress ->
            (round progress |> String.fromInt) ++ "% uploaded"

        Uploaded ->
            "Uploaded"

        ErrorWhileUploading _ ->
            "Problem uploading"


uploadedFilesDisplay : List UploadedFile -> Html Msg
uploadedFilesDisplay uploadedFiles =
    div []
        (uploadedFiles
            |> List.map .name
            |> List.map text
        )



-- keyedFileUploaders : Flags -> FilesBeingUploaded -> Html Msg
-- keyedFileUploaders flags filesBeingUploaded =
--     Html.Keyed.node "div"
--         []
--         (List.map
--             (\fileBeingUploaded -> Tuple.pair (fileBeingUploaded.id |> String.fromInt) (fileUploader flags fileBeingUploaded))
--             filesBeingUploaded
--         )
-- fileUploader : Flags -> FileBeingUploaded -> Html Msg
-- fileUploader flags fileBeingUploaded =
--     Html.node "file-uploader"
--         [ property "fbAuth" flags.fbAuth
--         , property "fileId" (Encode.int fileBeingUploaded.id)
--         , property "file" fileBeingUploaded.value
--         , Html.Events.on "fileUploadProgressed" <|
--             Decode.map2 FileUploadProgressed
--                 (Decode.succeed fileBeingUploaded.id)
--                 (Decode.at [ "detail", "progress" ] Decode.float)
--         , Html.Events.on "fileUploadErrored" (Decode.succeed <| FileUploadErrored fileBeingUploaded.id)
--         , Html.Events.on "fileUploadCompleted" (Decode.succeed <| FileUploadCompleted fileBeingUploaded.id)
--         ]
--         []
-- keyedTextGetters : FilesBeingRead -> Html Msg
-- keyedTextGetters filesBeingRead =
--     Html.Keyed.node
--         "div"
--         []
--         (List.map
--             (\fileToProcess -> Tuple.pair (fileToProcess.id |> String.fromInt) (textGetter fileToProcess))
--             filesBeingRead
--         )
-- textGetter : FileBeingRead -> Html Msg
-- textGetter fileBeingRead =
--     Html.node "text-getter"
--         [ property "fileId" (Encode.int fileBeingRead.id)
--         , property "file" fileBeingRead.value
--         , Html.Events.on "readText"
--             (Decode.map2 FileTextRead
--                 (Decode.succeed fileBeingRead.id)
--                 (Decode.at [ "details", "text" ] string)
--             )
--         ]
--         []


reportsDisplay : Reports -> Html Msg
reportsDisplay reports =
    div
        []
        (div [] [ text "Reports" ] :: List.map reportDisplay reports)


reportDisplay : Report -> Html Msg
reportDisplay report =
    div
        []
        [ text report.name
        , div []
            [ text
                (String.join " : "
                    [ report.size |> String.fromInt
                    , report.uploadedOn |> getDateFromPosix
                    ]
                )
            ]
        ]


getDateFromPosix : Posix -> String
getDateFromPosix posix =
    String.join " "
        [ Time.toYear Time.utc posix |> String.fromInt
        , Time.toMonth Time.utc posix |> getMonthNameFromMonth
        , Time.toDay Time.utc posix |> String.fromInt
        ]


getMonthNameFromMonth : Time.Month -> String
getMonthNameFromMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    on "dragstart" (Decode.succeed msg)


onDragOver : Msg -> Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" (Decode.succeed ( msg, True ))


onDrop : Msg -> Attribute Msg
onDrop msg =
    preventDefaultOn "drop" (Decode.succeed ( msg, True ))


onFilesDrop : (List Value -> Msg) -> Attribute Msg
onFilesDrop msg =
    preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))


filesDecoder : Decoder (List Value)
filesDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.list Decode.value)


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" (Decode.succeed msg)
