port module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (classList, placeholder, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Encode as Encode exposing (Value)
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


type FileError
    = None
    | InvalidFile
    | UploadError


type alias FileToProcess =
    { id : Int
    , file : Value
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
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { screenToShow = Login
      , email = "t@t.com"
      , password = "tester"
      , loginErrors = []
      , fullName = ""
      , signUpErrors = []
      , lastUsedFileId = 0
      , filesToProcess = []
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
    | CredentialsVerified Bool
      --  Reports messages
    | SearchTyped String
    | FilesDropped (List Value)
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

        CredentialsVerified value ->
            ( { model
                | screenToShow =
                    if value then
                        Display

                    else
                        Login
              }
            , Cmd.none
            )

        SearchTyped value ->
            ( { model | searchTerm = value }, Cmd.none )

        FilesDropped files ->
            let
                imageFiles : List Value
                imageFiles =
                    List.filter checkIfImageFile files

                fileIds : List Int
                fileIds =
                    List.range (model.lastUsedFileId + 1) (model.lastUsedFileId + List.length imageFiles)

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
                        imageFiles

                commandsToProcessFiles : List (Cmd msg)
                commandsToProcessFiles =
                    List.map
                        (\fileToProcess ->
                            Encode.object
                                [ ( "id", Encode.int fileToProcess.id )
                                , ( "file", fileToProcess.file )
                                ]
                                |> processAFile
                        )
                        filesToProcess
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


checkIfImageFile : Value -> Bool
checkIfImageFile file =
    case decodeFile file of
        Just f ->
            List.member (File.mime f) [ "image/png", "image/jpeg", "application/pdf" ]

        Nothing ->
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


decodeFile : Value -> Maybe File
decodeFile value =
    case Decode.decodeValue File.decoder value of
        Ok f ->
            Just f

        Err _ ->
            Nothing



-- Ports
-- Port Outs


port signInAUser :
    { email : String, password : String }
    -> Cmd msg --  Since we are not expecting any message in return, the return type is the lowercase 'msg'


port registerAUser : { email : String, name : String } -> Cmd msg


port processAFile : Value -> Cmd msg



-- Port Ins


port credentialsVerified : (Bool -> msg) -> Sub msg


port uploadProgress : (Value -> msg) -> Sub msg


port uploadError : (Value -> msg) -> Sub msg


port uploadComplete : (Value -> msg) -> Sub msg


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
    Decode.map2 FileUploadCompletion
        (Decode.field "id" Decode.int)
        (Decode.field "path" Decode.string)


decodeFileUploadCompletion : Value -> Msg
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
            Sub.batch [ credentialsVerified CredentialsVerified ]

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
        [ filesDropZone model.filesToProcess
        , input [ placeholder "Search", onInput SearchTyped ] []
        ]


filesDropZone : List FileToProcess -> Html Msg
filesDropZone filesToProcess =
    div
        [ onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        (List.append [ div [] [ text "Drag a report here to upload" ] ] (List.map fileDisplay filesToProcess))


fileDisplay : FileToProcess -> Html Msg
fileDisplay fileToProcess =
    div
        []
        [ fileToProcess.id |> String.fromInt |> text
        , text " - "
        , uploadStatus fileToProcess.progress fileToProcess.error |> text
        ]


uploadStatus : Float -> FileError -> String
uploadStatus progress error =
    case progress * 100 |> round of
        0 ->
            case error of
                None ->
                    "Starting upload"

                InvalidFile ->
                    "This is not a valid file and will not be uploaded"

                UploadError ->
                    "Couldn't upload"

        100 ->
            "Uploaded"

        _ ->
            (progress
                |> (*) 100
                |> round
                |> String.fromInt
            )
                ++ "% uploaded"
