port module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (Attribute, Html, button, div, h1, input, text)
import Html.Attributes exposing (classList, placeholder, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Encode as Encode
import Set
import Time
import Validate exposing (Validator)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Screen
    = Login
    | SignUp
    | Display


type StatusOfReport
    = Uploading
    | AnErrorOccurredWhileUploading
    | Waiting
    | Processing
    | Processed
    | AnErrorOccurredWhileProcessing


type alias Report =
    { id : String
    , name : String
    , date : Time.Posix
    , status : StatusOfReport
    , thumbnailPath : String
    , readings : List Reading
    }


type Result
    = Quantity Float
    | Quality String


type alias Reading =
    { kind : String
    , result : Result
    }


type FileError
    = None
    | InvalidFile
    | UploadError


type alias FileToProcess =
    { id : Int
    , file : Encode.Value
    , progress : Float
    , error : FileError --  For now, we don't worry about what error it was
    , path : String
    }


type alias FileUploadProgress =
    { id : Int, progress : Float }


type alias FileUploadError =
    { id : Int }


type alias FileUploadCompletion =
    { id : Int, path : String }


type alias Model =
    { -- Login and Sign up stuff
      screenToShow : Screen
    , email : String
    , password : String
    , loginErrors : List String
    , fullName : String
    , signUpErrors : List String

    --  Reports display stuff
    , lastUsedFileId : Int
    , filesToProcess : List FileToProcess
    , searchTerm : String
    , reports : List Report
    , selectedReport : Maybe Report
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { screenToShow = Display
      , email = ""
      , password = ""
      , loginErrors = []
      , fullName = ""
      , signUpErrors = []
      , lastUsedFileId = 0
      , filesToProcess = []
      , searchTerm = ""
      , reports = []
      , selectedReport = Nothing
      }
    , Cmd.none
    )


type
    Msg
    --  Login and registration messages
    = EmailTyped String
    | PasswordTyped String
    | LoginButtonClicked
    | SignUpButtonOnLoginClicked
    | FullNameTyped String
    | SignUpButtonClicked
    | LoginButtonOnSignUpClicked
      --  Reports messages
    | SearchTyped String
    | FilesDropped (List Encode.Value)
    | FileUploadProgressed FileUploadProgress
    | FileUploadErrored FileUploadError
    | FileUploadCompleted FileUploadCompletion
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

        SearchTyped value ->
            ( { model | searchTerm = value }, Cmd.none )

        FilesDropped files ->
            let
                admissibleFiles : List Encode.Value
                admissibleFiles =
                    files
                        |> List.filter
                            (\file ->
                                case decodeFile file of
                                    Just f ->
                                        List.member (File.mime f) [ "image/png", "image/jpeg", "application/pdf" ]

                                    Nothing ->
                                        False
                            )

                fileIds : List Int
                fileIds =
                    List.range (model.lastUsedFileId + 1) (model.lastUsedFileId + List.length admissibleFiles)

                filesToProcess : List FileToProcess
                filesToProcess =
                    List.map2
                        (\index file ->
                            { id = index
                            , file = file
                            , progress = 0
                            , error = None
                            , path = ""
                            }
                        )
                        fileIds
                        admissibleFiles

                commandsToProcessFiles : List (Cmd msg)
                commandsToProcessFiles =
                    List.map (\fileToProcess -> processAFile { id = fileToProcess.id, file = fileToProcess.file }) filesToProcess
            in
            ( { model
                | filesToProcess = List.append model.filesToProcess filesToProcess
                , lastUsedFileId = List.maximum fileIds |> Maybe.withDefault 0
              }
            , Cmd.batch commandsToProcessFiles
            )

        FileUploadProgressed fileUploadProgress ->
            let
                updateIfCorrectFile : FileUploadProgress -> FileToProcess -> FileToProcess
                updateIfCorrectFile progress file =
                    if file.id == progress.id then
                        { file | progress = progress.progress }

                    else
                        file

                updatedFilesToProcess : List FileToProcess
                updatedFilesToProcess =
                    model.filesToProcess
                        |> List.map (updateIfCorrectFile fileUploadProgress)
            in
            ( { model | filesToProcess = updatedFilesToProcess }, Cmd.none )

        FileUploadErrored fileUploadError ->
            let
                updateIfCorrectFile : FileUploadError -> FileToProcess -> FileToProcess
                updateIfCorrectFile error file =
                    if file.id == error.id then
                        { file | error = UploadError }

                    else
                        file

                updatedFilesToProcess : List FileToProcess
                updatedFilesToProcess =
                    model.filesToProcess
                        |> List.map (updateIfCorrectFile fileUploadError)
            in
            ( { model | filesToProcess = updatedFilesToProcess }, Cmd.none )

        FileUploadCompleted fileUploadCompletion ->
            let
                updateIfCorrectFile : FileUploadCompletion -> FileToProcess -> FileToProcess
                updateIfCorrectFile completion file =
                    if file.id == completion.id then
                        { file | progress = 1, path = completion.path }

                    else
                        file

                updatedFilesToProcess : List FileToProcess
                updatedFilesToProcess =
                    model.filesToProcess
                        |> List.map (updateIfCorrectFile fileUploadCompletion)
            in
            ( { model | filesToProcess = updatedFilesToProcess }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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


decodeFile : Encode.Value -> Maybe File
decodeFile value =
    case Decode.decodeValue File.decoder value of
        Ok f ->
            Just f

        Err _ ->
            Nothing


port signInAUser :
    { email : String, password : String }
    -> Cmd msg --  Since we are not expecting any message in return, the return type is the lowercase 'msg'


port registerAUser : { email : String, name : String } -> Cmd msg


port processAFile : { id : Int, file : Encode.Value } -> Cmd msg


port uploadProgress : (Encode.Value -> msg) -> Sub msg


port uploadError : (Encode.Value -> msg) -> Sub msg


port uploadComplete : (Encode.Value -> msg) -> Sub msg


fileUploadProgressDecoder : Decoder FileUploadProgress
fileUploadProgressDecoder =
    Decode.map2 FileUploadProgress
        (Decode.field "id" Decode.int)
        (Decode.field "progress" Decode.float)


decodeFileUploadProgress : Encode.Value -> Msg
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


decodeFileUploadError : Encode.Value -> Msg
decodeFileUploadError value =
    case Decode.decodeValue fileUploadErroredDecoder value of
        Ok fileUploadError ->
            FileUploadErrored fileUploadError

        Err _ ->
            NoOp


fileUploadCompletionDecoder : Decoder FileUploadCompletion
fileUploadCompletionDecoder =
    Decode.map2 FileUploadCompletion
        (Decode.field "id" Decode.int)
        (Decode.field "path" Decode.string)


decodeFileUploadCompletion : Encode.Value -> Msg
decodeFileUploadCompletion value =
    case Decode.decodeValue fileUploadCompletionDecoder value of
        Ok fileUploadCompletion ->
            FileUploadCompleted fileUploadCompletion

        Err _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screenToShow of
        Login ->
            Sub.none

        SignUp ->
            Sub.none

        Display ->
            Sub.batch
                [ uploadProgress decodeFileUploadProgress
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


onFilesDrop : (List Encode.Value -> Msg) -> Attribute Msg
onFilesDrop msg =
    preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))


filesDecoder : Decoder (List Encode.Value)
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
        [ filesDisplay model.filesToProcess
        , input [ placeholder "Search", onInput SearchTyped ] []
        , reportsDisplay model.reports
        , readingsDisplay model.reports
        ]


filesDisplay : List FileToProcess -> Html Msg
filesDisplay filesToProcess =
    div
        [ onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        (List.append [ div [] [ text "Drag a report here to upload" ] ] (List.map fileDisplay filesToProcess))


fileDisplay : FileToProcess -> Html Msg
fileDisplay fileToProcess =
    div []
        [ fileToProcess.id |> String.fromInt |> text
        , text " - "
        , fileToProcess.progress |> String.fromFloat |> text
        ]


reportsDisplay : List Report -> Html Msg
reportsDisplay reports =
    div []
        [ h1 [] [ text "Reports display" ]
        , div
            []
            (reports
                |> List.sortBy (\a -> a.date |> Time.posixToMillis)
                |> List.map reportDisplay
            )
        ]


reportDisplay : Report -> Html Msg
reportDisplay report =
    div []
        [ div [] [ report.date |> Time.posixToMillis |> String.fromInt |> text ]
        , div [] [ text report.name ]
        ]


readingsDisplay : List Report -> Html Msg
readingsDisplay reports =
    let
        --  Firstly, get all the values from all the reports
        --  Make a unique set of reading kinds
        kinds : List String
        kinds =
            reports
                |> List.map (\report -> List.map (\reading -> reading.kind) report.readings)
                |> List.concat
                |> Set.fromList
                |> Set.toList
                |> List.sort
    in
    div []
        [ div [] (List.map (\kind -> div [] [ text kind ]) kinds)
        , div [] [ text "Graph" ]
        ]
