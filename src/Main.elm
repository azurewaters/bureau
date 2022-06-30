port module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, property, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Keyed
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Encode as Encode exposing (Value)
import Process
import Task
import Time exposing (Posix)
import Validate exposing (Validator)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Flags =
    { pdfjs : Value
    , fbAuth : Value
    , fbStore : Value
    , fbStorage : Value
    }


type alias Model =
    { -- Objects from JS
      flags : Flags

    -- Login and Sign up stuff
    , screenToShow : Screen
    , email : String
    , password : String
    , loginErrors : Errors
    , fullName : String
    , signUpErrors : Errors
    , currentUsersId : String

    -- Files being uploaded and processed
    , lastUsedIndexNumber : Int
    , filesBeingUploaded : FilesBeingUploaded

    --  Reports display stuff
    , reports : Reports
    , searchTerm : String
    }


type Screen
    = Login
    | SignUp
    | Display


type alias Errors =
    List String


type alias FileStoragePath =
    String


type UploadStatus
    = Uploading Float
    | Completed
    | Error String


type alias FileBeingUploaded =
    { id : Int
    , name : String
    , size : Int
    , mime : String
    , value : Value
    , uploadStatus : UploadStatus
    }


type alias FilesBeingUploaded =
    List FileBeingUploaded


type alias Report =
    { id : String
    , name : String
    , size : Int
    , mime : String
    , uploadedOn : Posix
    }


type alias Reports =
    List Report


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { flags = flags
      , screenToShow = Login
      , email = "t@t.com"
      , password = "tester"
      , loginErrors = []
      , fullName = ""
      , signUpErrors = []
      , currentUsersId = ""
      , reports = []
      , lastUsedIndexNumber = 0
      , filesBeingUploaded = []
      , searchTerm = ""
      }
    , Cmd.none
    )


type Msg
    = --  Login and registration messages
      EmailTyped String
    | PasswordTyped String
    | LoginButtonClicked
    | SignUpButtonOnLoginClicked
    | FullNameTyped String
    | SignUpButtonClicked
    | LoginButtonOnSignUpClicked
    | CredentialsVerified String
      --  Reports messages
    | ReportsFetched Reports
    | FilesDropped (List Value)
    | FileUploadProgressed Int Float
    | FileUploadErrored Int
    | FileUploadCompleted Int
    | RemoveUploadedFile Int
      --  Miscellaneous
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailTyped email ->
            ( { model | email = email }, Cmd.none )

        PasswordTyped password ->
            ( { model | password = password }, Cmd.none )

        LoginButtonClicked ->
            case Validate.validate loginValidator model of
                Ok _ ->
                    ( { model | loginErrors = [] }, signInAUser { email = model.email, password = model.password } )

                Err errors ->
                    ( { model | loginErrors = errors }, Cmd.none )

        SignUpButtonOnLoginClicked ->
            ( { model | screenToShow = SignUp }, Cmd.none )

        FullNameTyped fullName ->
            ( { model | fullName = fullName }, Cmd.none )

        SignUpButtonClicked ->
            case Validate.validate signUpValidator model of
                Ok _ ->
                    ( { model | signUpErrors = [] }, registerAUser { email = model.email, name = model.fullName } )

                Err errors ->
                    ( { model | signUpErrors = errors }, Cmd.none )

        LoginButtonOnSignUpClicked ->
            ( { model | screenToShow = Login }, Cmd.none )

        CredentialsVerified value ->
            ( { model
                | currentUsersId = value
                , screenToShow =
                    if value /= "" then
                        Display

                    else
                        Login
              }
            , fetchTheUsersReports value
            )

        ReportsFetched reports ->
            let
                _ =
                    Debug.log "ReportsFetched"
            in
            ( { model | reports = reports }, Cmd.none )

        FilesDropped values ->
            let
                imageFileValues : List ( File, Value )
                imageFileValues =
                    List.filterMap
                        (\value ->
                            case Decode.decodeValue File.decoder value of
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
                    List.range model.lastUsedIndexNumber (model.lastUsedIndexNumber + List.length uniqueImageFileValues)

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
            in
            ( { model
                | filesBeingUploaded = List.append model.filesBeingUploaded newFilesBeingUploaded
                , lastUsedIndexNumber = model.lastUsedIndexNumber + List.length uniqueImageFileValues
              }
            , Cmd.none
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
                        (\fileBeingUploaded -> { fileBeingUploaded | uploadStatus = Error "Error" })
              }
            , Cmd.none
            )

        FileUploadCompleted id ->
            ( { model
                | filesBeingUploaded =
                    updateFilesBeingUploaded
                        model.filesBeingUploaded
                        id
                        (\fileBeingUploaded -> { fileBeingUploaded | uploadStatus = Completed })
              }
            , Task.perform (\_ -> RemoveUploadedFile id) (Process.sleep 3000)
            )

        RemoveUploadedFile id ->
            ( { model | filesBeingUploaded = List.filter (\fileBeingUploaded -> fileBeingUploaded.id /= id) model.filesBeingUploaded }, Cmd.none )

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


port signInAUser :
    { email : String, password : String }
    -> Cmd msg --  Since we are not expecting any message in return, the return type is the lowercase 'msg'


port registerAUser : { email : String, name : String } -> Cmd msg


port processAFile : Value -> Cmd msg


port fetchedUsersReports : (Value -> msg) -> Sub msg



-- Port Ins


port credentialsVerified : (String -> msg) -> Sub msg


port uploadProgress : (Value -> msg) -> Sub msg


port uploadError : (Value -> msg) -> Sub msg


port uploadComplete : (Value -> msg) -> Sub msg


port fetchTheUsersReports : String -> Cmd msg


reportDecoder : Decoder Report
reportDecoder =
    Decode.map5 Report
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "size" Decode.int)
        (Decode.field "mime" Decode.string)
        (Decode.field "uploadedOn" Decode.int |> Decode.map Time.millisToPosix)


reportsDecoder : Decoder Reports
reportsDecoder =
    Decode.list reportDecoder


decodeReports : Value -> Msg
decodeReports value =
    case Decode.decodeValue reportsDecoder value of
        Ok reports ->
            ReportsFetched reports

        Err _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screenToShow of
        Login ->
            Sub.batch [ credentialsVerified CredentialsVerified ]

        SignUp ->
            Sub.none

        Display ->
            Sub.batch
                [ fetchedUsersReports decodeReports
                ]



--  VIEWS


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
        , button [ onClick SignUpButtonOnLoginClicked, class "btn" ] [ text "Sign Up" ]
        , button [ onClick LoginButtonClicked, class "btn btn-primary" ] [ text "Login" ]
        , div [] (List.map (\error -> div [] [ text error ]) model.loginErrors)
        ]


signUp : Model -> Html Msg
signUp model =
    div []
        [ input [ type_ "email", required True, placeholder "Your email address", class "input", onInput EmailTyped, value model.email ] []
        , input [ type_ "string", required True, placeholder "Your full name", class "input", onInput FullNameTyped, value model.fullName ] []
        , button [ onClick LoginButtonOnSignUpClicked, class "btn btn-primary" ] [ text "Login" ]
        , button [ onClick SignUpButtonClicked, class "btn" ] [ text "Sign up" ]
        , div [] (List.map (\error -> div [] [ text error ]) model.signUpErrors)
        ]


display : Model -> Html Msg
display model =
    div
        []
        [ filesDropZone model.flags model.filesBeingUploaded
        , reportsDisplay model.reports
        ]


filesDropZone : Flags -> FilesBeingUploaded -> Html Msg
filesDropZone flags filesBeingUploaded =
    div
        [ onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        (List.concat
            [ [ div [] [ text "Drag a report here to upload" ] ]
            , List.map fileDisplay filesBeingUploaded
            , [ keyedFileUploaders flags filesBeingUploaded ]
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

        Completed ->
            "Uploaded"

        Error _ ->
            "Problem uploading"


keyedFileUploaders : Flags -> FilesBeingUploaded -> Html Msg
keyedFileUploaders flags filesBeingUploaded =
    Html.Keyed.node "div"
        []
        (List.map
            (\fileBeingUploaded -> Tuple.pair (fileBeingUploaded.id |> String.fromInt) (fileUploader flags fileBeingUploaded))
            filesBeingUploaded
        )


fileUploader : Flags -> FileBeingUploaded -> Html Msg
fileUploader flags fileBeingUploaded =
    Html.node "file-uploader"
        [ property "fbAuth" flags.fbAuth
        , property "fbStore" flags.fbStore
        , property "fbStorage" flags.fbStorage
        , property "fileId" (Encode.int fileBeingUploaded.id)
        , property "file" fileBeingUploaded.value
        , Html.Events.on "fileUploadProgressed" <|
            Decode.map2 FileUploadProgressed
                (Decode.succeed fileBeingUploaded.id)
                (Decode.at [ "detail", "progress" ] Decode.float)
        , Html.Events.on "fileUploadErrored" (Decode.succeed <| FileUploadErrored fileBeingUploaded.id)
        , Html.Events.on "fileUploadCompleted" (Decode.succeed <| FileUploadCompleted fileBeingUploaded.id)
        ]
        []


keyedTextGetters : Value -> List FileBeingUploaded -> Html Msg
keyedTextGetters pdfjs filesToProcess =
    Html.Keyed.node
        "div"
        []
        (List.map
            (\fileToProcess -> Tuple.pair (fileToProcess.id |> String.fromInt) (textGetter pdfjs fileToProcess))
            filesToProcess
        )


textGetter : Value -> FileBeingUploaded -> Html Msg
textGetter pdfjs fileToProcess =
    Html.node "text-getter"
        [ property "pdfjs" pdfjs
        , property "fileId" (Encode.int fileToProcess.id)
        , property "file" fileToProcess.value

        -- , Html.Events.on "gotText"
        --     (Decode.map GotTextInFile <|
        --         Decode.at [ "details", "text" ] Decode.string
        --     )
        ]
        []


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
