port module Main exposing (..)

import Browser
import File
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (classList, placeholder, property, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Keyed
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Encode as Encode exposing (Value)
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


type Screen
    = Login
    | SignUp
    | Display


type alias Errors =
    List String


type alias FileStoragePath =
    String


type FileUploadStatus
    = Uploading Float
    | Completed
    | Error String


type alias FileBeingProcessed =
    { id : Int
    , name : String
    , size : Int
    , mime : String
    , value : Value
    , uploadStatus : FileUploadStatus
    }


type alias FileUploadProgress =
    { id : Int, progress : Float }


type alias FileUploadError =
    { id : Int }


type alias FileUploadCompletion =
    { id : Int }


type alias Report =
    { id : String
    , name : String
    , size : Int
    , mime : String
    , uploadedOn : Posix
    }


type alias Reports =
    List Report


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

    --  Reports display stuff
    , reports : Reports
    , lastUsedFileId : Int
    , filesBeingProcessed : List FileBeingProcessed
    , searchTerm : String

    -- Other
    , textInFile : String
    }


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
      , lastUsedFileId = 0
      , filesBeingProcessed = []
      , searchTerm = ""
      , textInFile = ""
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
    | FileUploadProgressed FileUploadProgress
    | FileUploadErrored FileUploadError
    | FileUploadCompleted FileUploadCompletion
      --  Miscellaneous
    | GotTextInFile String
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
                uniqueImageValues : List Value
                uniqueImageValues =
                    values
                        |> List.filter checkIfFileIsAnImage
                        |> List.filter (checkIfFileIsAlreadyBeingProcessed model.filesBeingProcessed)

                fileIds : List Int
                fileIds =
                    List.range (model.lastUsedFileId + 1) (List.length uniqueImageValues + model.lastUsedFileId)

                filesToProcess : List FileBeingProcessed
                filesToProcess =
                    List.indexedMap
                        (\index value ->
                            case Decode.decodeValue File.decoder value of
                                Ok file ->
                                    { id = model.lastUsedFileId + index
                                    , name = File.name file
                                    , size = File.size file
                                    , mime = File.mime file
                                    , value = value
                                    , uploadStatus = Uploading 0.0
                                    }

                                Err _ ->
                                    { id = -1
                                    , name = ""
                                    , size = 0
                                    , mime = ""
                                    , value = value
                                    , uploadStatus = Error "Could not decode file"
                                    }
                        )
                        uniqueImageValues
                        |> List.filter (\f -> f.id > -1)

                commandsToProcessFiles : List (Cmd msg)
                commandsToProcessFiles =
                    List.map
                        (\fileToProcess ->
                            Encode.object
                                [ ( "id", Encode.int fileToProcess.id )
                                , ( "file", fileToProcess.value )
                                ]
                                |> processAFile
                        )
                        filesToProcess
            in
            ( { model
                | filesBeingProcessed = List.append model.filesBeingProcessed filesToProcess
                , lastUsedFileId = List.maximum fileIds |> Maybe.withDefault 0
              }
            , Cmd.batch commandsToProcessFiles
            )

        FileUploadProgressed fileUploadProgress ->
            let
                updateIfCorrectFile : FileUploadProgress -> FileBeingProcessed -> FileBeingProcessed
                updateIfCorrectFile progress file =
                    if file.id == progress.id then
                        { file | uploadStatus = Uploading progress.progress }

                    else
                        file

                updatedFilesToProcess : List FileBeingProcessed
                updatedFilesToProcess =
                    model.filesBeingProcessed
                        |> List.map (updateIfCorrectFile fileUploadProgress)
            in
            ( { model | filesBeingProcessed = updatedFilesToProcess }, Cmd.none )

        FileUploadErrored fileUploadError ->
            let
                updateIfCorrectFile : FileUploadError -> FileBeingProcessed -> FileBeingProcessed
                updateIfCorrectFile error file =
                    if file.id == error.id then
                        { file | uploadStatus = Error "" }

                    else
                        file

                updatedFilesToProcess : List FileBeingProcessed
                updatedFilesToProcess =
                    model.filesBeingProcessed
                        |> List.map (updateIfCorrectFile fileUploadError)
            in
            ( { model | filesBeingProcessed = updatedFilesToProcess }, Cmd.none )

        FileUploadCompleted fileUploadCompletion ->
            let
                updateIfCorrectFile : FileUploadCompletion -> FileBeingProcessed -> FileBeingProcessed
                updateIfCorrectFile completion file =
                    if file.id == completion.id then
                        { file | uploadStatus = Completed }

                    else
                        file

                updatedFilesToProcess : List FileBeingProcessed
                updatedFilesToProcess =
                    model.filesBeingProcessed
                        |> List.map (updateIfCorrectFile fileUploadCompletion)
            in
            ( { model | filesBeingProcessed = updatedFilesToProcess }, Cmd.none )

        GotTextInFile t ->
            ( { model | textInFile = t }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


checkIfFileIsAnImage : Value -> Bool
checkIfFileIsAnImage value =
    case Decode.decodeValue File.decoder value of
        Ok file ->
            List.member (File.mime file) [ "image/png", "image/jpeg", "application/pdf" ]

        _ ->
            False


checkIfFileIsAlreadyBeingProcessed : List FileBeingProcessed -> Value -> Bool
checkIfFileIsAlreadyBeingProcessed filesAlreadyBeingProcessed value =
    case Decode.decodeValue File.decoder value of
        Ok file ->
            List.all (\fbp -> fbp.name /= File.name file) filesAlreadyBeingProcessed

        _ ->
            False


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


fileUploadProgressDecoder : Decoder FileUploadProgress
fileUploadProgressDecoder =
    Decode.map2 FileUploadProgress
        (Decode.field "id" Decode.int)
        (Decode.field "progress" Decode.float)


decodeFileUploadProgress : Value -> Msg
decodeFileUploadProgress value =
    case Decode.decodeValue fileUploadProgressDecoder value of
        Ok fileUploadProgress ->
            FileUploadProgressed fileUploadProgress

        Err _ ->
            NoOp


fileUploadErroredDecoder : Decoder FileUploadError
fileUploadErroredDecoder =
    Decode.map FileUploadError
        (Decode.field "id" Decode.int)


decodeFileUploadError : Value -> Msg
decodeFileUploadError value =
    case Decode.decodeValue fileUploadErroredDecoder value of
        Ok fileUploadError ->
            FileUploadErrored fileUploadError

        Err _ ->
            NoOp


fileUploadCompletionDecoder : Decoder FileUploadCompletion
fileUploadCompletionDecoder =
    Decode.map FileUploadCompletion
        (Decode.field "id" Decode.int)


decodeFileUploadCompletion : Value -> Msg
decodeFileUploadCompletion value =
    case Decode.decodeValue fileUploadCompletionDecoder value of
        Ok fileUploadCompletion ->
            FileUploadCompleted fileUploadCompletion

        Err _ ->
            NoOp


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
                , uploadProgress decodeFileUploadProgress
                , uploadError decodeFileUploadError
                , uploadComplete decodeFileUploadCompletion
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


classes : String -> Html.Attribute msg
classes stringOfClasses =
    stringOfClasses
        |> String.split " "
        |> List.map (\x -> ( x, True ))
        |> classList


inputClasses : Html.Attribute msg
inputClasses =
    classes "border border-gray-300 rounded px-4 py-2"


buttonClasses : Html.Attribute msg
buttonClasses =
    classes "bg-gray-900 border border-gray-900 rounded px-4 py-2 text-white font-semibold"


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
        [ input [ type_ "email", required True, placeholder "Your email address", inputClasses, onInput EmailTyped, value model.email ] []
        , input [ type_ "password", required True, placeholder "Your password", inputClasses, onInput PasswordTyped, value model.password ] []
        , button [ onClick SignUpButtonOnLoginClicked, buttonClasses ] [ text "Sign Up" ]
        , button [ onClick LoginButtonClicked, buttonClasses ] [ text "Login" ]
        , div [] (List.map (\error -> div [] [ text error ]) model.loginErrors)
        ]


signUp : Model -> Html Msg
signUp model =
    div []
        [ input [ type_ "email", required True, placeholder "Your email address", inputClasses, onInput EmailTyped, value model.email ] []
        , input [ type_ "string", required True, placeholder "Your full name", inputClasses, onInput FullNameTyped, value model.fullName ] []
        , button [ onClick LoginButtonOnSignUpClicked, buttonClasses ] [ text "Login" ]
        , button [ onClick SignUpButtonClicked, buttonClasses ] [ text "Sign up" ]
        , div [] (List.map (\error -> div [] [ text error ]) model.signUpErrors)
        ]


display : Model -> Html Msg
display model =
    div
        []
        [ filesDropZone model.flags.pdfjs model.filesBeingProcessed
        , text model.textInFile
        , reportsDisplay model.reports
        ]


filesDropZone : Value -> List FileBeingProcessed -> Html Msg
filesDropZone pdfjs filesToProcess =
    div
        [ onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        (List.concat
            [ [ div [] [ text "Drag a report here to upload" ] ]
            , List.map fileDisplay filesToProcess
            , [ keyedTextGetters pdfjs filesToProcess ]
            ]
        )


fileDisplay : FileBeingProcessed -> Html Msg
fileDisplay fileToProcess =
    div
        []
        [ fileToProcess.id |> String.fromInt |> text
        , text " - "
        , getUploadStatus fileToProcess |> text
        ]


getUploadStatus : FileBeingProcessed -> String
getUploadStatus fileToProcess =
    case fileToProcess.uploadStatus of
        Uploading progress ->
            (round progress |> String.fromInt) ++ "% uploaded"

        Completed ->
            "Uploaded"

        Error _ ->
            "Problem uploading"


keyedTextGetters : Value -> List FileBeingProcessed -> Html Msg
keyedTextGetters pdfjs filesToProcess =
    Html.Keyed.node
        "div"
        []
        (List.map
            (\fileToProcess -> Tuple.pair (fileToProcess.id |> String.fromInt) (textGetter pdfjs fileToProcess))
            filesToProcess
        )


textGetter : Value -> FileBeingProcessed -> Html Msg
textGetter pdfjs fileToProcess =
    Html.node "text-getter"
        [ property "pdfjs" pdfjs
        , property "fileId" (Encode.int fileToProcess.id)
        , property "file" fileToProcess.value
        , Html.Events.on "gotText"
            (Decode.map GotTextInFile <|
                Decode.at [ "details", "text" ] Decode.string
            )
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
