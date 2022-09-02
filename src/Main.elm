module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Attribute, Html, a, button, div, h1, h2, h3, input, li, ol, p, progress, span, text)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Http exposing (emptyBody, expectBytesResponse, expectJson, expectWhatever, fileBody, header, request)
import Json.Decode as Decode exposing (Decoder, Error(..), field, int, string)
import Json.Encode as Encode
import Process
import Set
import Task
import Time exposing (Posix, Zone)
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing (Parser)
import Validate exposing (Validator)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- Some constants


supabaseURL : String
supabaseURL =
    "https://uwiqgkdwjhdlmjyistoq.supabase.co"


supabaseAnonymousToken : String
supabaseAnonymousToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InV3aXFna2R3amhkbG1qeWlzdG9xIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NTgxMzIwMjMsImV4cCI6MTk3MzcwODAyM30.d7p38XejbnWMALNPmf8wACaLdhc6TM2AKSdGTWJNJrA"


type alias Model =
    { --  General stuff
      key : Key
    , url : Url
    , supabaseAuthorisedToken : String
    , currentPage : Page
    , currentTimezone : Zone

    -- Login and Sign up stuff
    , email : String
    , password : String
    , currentUsersId : Maybe String
    , signUpStatus : SignUpStatus

    -- Home stuff
    , selectedReportIds : List ReportId
    , showDeleteReportsConfirmation : Bool

    -- User's data
    , reports : List Report
    , errors : Errors

    -- Files being uploaded
    , lastUsedIndexNumberForFilesBeingUploaded : Int
    , filesBeingUploaded : List FileBeingUploaded
    }


type Page
    = Top
    | Login
    | SignUp
    | Home
    | NotFound


urlParser : Parser (Page -> a) a
urlParser =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map Login (Parser.s "login")
        , Parser.map SignUp (Parser.s "signup")
        , Parser.map Home (Parser.s "home")
        ]


urlToPage : Url -> Page
urlToPage url =
    Parser.parse urlParser url
        |> Maybe.withDefault NotFound


type alias Errors =
    List String


type alias FileBeingUploaded =
    { id : Int
    , file : File
    , uploadStatus : UploadStatus
    }


type UploadStatus
    = Uploading Int Int
    | Uploaded
    | ErrorWhileUploading String


type alias FileUploadedResponse =
    { key : String }


type alias AccessDetails =
    { accessToken : String, userId : String }


type alias Report =
    { id : ReportId
    , name : String
    , size : Int
    , mime : String
    , uploadedOn : Posix
    }


type alias ReportId =
    Int


type SignUpStatus
    = SignUpStatusDefault
    | SignUpStatusConfirmationRequired


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { url = url
      , key = key
      , currentTimezone = Time.utc
      , supabaseAuthorisedToken = ""
      , currentPage = urlToPage url
      , email = "azurewaters@gmail.com"
      , password = "QcGmuuQgpEKpnsSpIqir"
      , currentUsersId = Nothing
      , signUpStatus = SignUpStatusDefault
      , selectedReportIds = []
      , showDeleteReportsConfirmation = False
      , reports = []
      , errors = []
      , lastUsedIndexNumberForFilesBeingUploaded = 0
      , filesBeingUploaded = []
      }
    , Task.perform GotTheTimeZone Time.here
    )


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest
    | GotTheTimeZone Zone
    | DoNothing (Result Http.Error ())
    | NoOp
    | DelistErrors Errors
      --  Login and registration messages
    | EmailTyped String
    | PasswordTyped String
    | LogInClicked
    | SignUpClicked
    | UserSignedIn (Result Http.Error AccessDetails)
    | FetchedUploadedFiles (Result Http.Error (List Report))
    | UserSignedUp (Result Http.Error (List String)) -- Identities
      -- Home messages
    | GoToLoginPageClicked
    | LogOutClicked
    | ChooseFilesClicked
    | FilesSelected File (List File)
    | UploadFilesCloseClicked
    | ToggleSelection ReportId
    | DeleteClicked
    | DeleteReportsConfirmationDeleteClicked
    | DeleteReportsConfirmationCloseClicked
    | ReportDeleted ReportId (Result Http.Error ())
    | DownloadClicked
    | DownloadedFile ReportId (Result Http.Error Bytes)
      --  File upload messages
    | FilesDropped (List File)
    | GotUploadProgress Int Http.Progress
    | FileUploaded FileBeingUploaded (Result Http.Error FileUploadedResponse)
    | InsertUploadedFilesDetails FileBeingUploaded Posix
    | InsertedUploadedFilesDetails (Result Http.Error (List Report))
    | IgnoreSuccessfulHttpRequests (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        GotTheTimeZone zone ->
            ( { model | currentTimezone = zone }, Cmd.none )

        EmailTyped email ->
            ( { model | email = email }, Cmd.none )

        PasswordTyped password ->
            ( { model | password = password }, Cmd.none )

        LogInClicked ->
            case Validate.validate loginValidator model of
                Ok _ ->
                    ( model, signInAUser model.email model.password )

                Err errors ->
                    ( { model | errors = errors }, pauseAndDelistErrors errors )

        SignUpClicked ->
            case Validate.validate signUpValidator model of
                Ok _ ->
                    ( model
                    , Http.request
                        { method = "POST"
                        , url = supabaseURL ++ "/auth/v1/signup"
                        , headers =
                            [ header "apikey" supabaseAnonymousToken
                            , header "Content-Type" "application/json"
                            ]
                        , body =
                            Http.jsonBody
                                (Encode.object
                                    [ ( "email", Encode.string model.email )
                                    , ( "password", Encode.string model.password )
                                    ]
                                )
                        , expect = Http.expectJson UserSignedUp signUpResponseDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Err errors ->
                    ( { model | errors = errors }, pauseAndDelistErrors errors )

        UserSignedIn result ->
            case result of
                Ok accessDetails ->
                    ( { model
                        | supabaseAuthorisedToken = accessDetails.accessToken
                        , currentUsersId = Just accessDetails.userId
                        , currentPage = Home
                      }
                    , Cmd.batch
                        [ Navigation.pushUrl model.key "home"
                        , fetchTheUsersUploadedFiles accessDetails.accessToken
                        ]
                    )

                Err _ ->
                    let
                        error =
                            "Unable to log in"
                    in
                    ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

        FetchedUploadedFiles value ->
            case value of
                Ok result ->
                    ( { model | reports = result }, Cmd.none )

                Err e ->
                    let
                        error =
                            getHttpErrorToDisplay e
                    in
                    ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

        UserSignedUp result ->
            case result of
                Ok ids ->
                    case ids of
                        [] ->
                            --  There was a problem signing up. Most likely, this user already exists.
                            let
                                error =
                                    "Couldn't sign you up. Try logging in."
                            in
                            ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

                        _ ->
                            --  Signed up fine. The user needs to be told to confirm their email.
                            ( { model | signUpStatus = SignUpStatusConfirmationRequired }, Cmd.none )

                Err e ->
                    let
                        error =
                            getHttpErrorToDisplay e
                    in
                    ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

        GoToLoginPageClicked ->
            ( { model | currentPage = Login }, Navigation.pushUrl model.key "login" )

        LogOutClicked ->
            ( model
            , request
                { method = "post"
                , url = supabaseURL ++ "/auth/v1/logout"
                , headers =
                    [ header "apikey" supabaseAnonymousToken
                    , header "Authorization" ("Bearer " ++ model.supabaseAuthorisedToken)
                    ]
                , body = Http.emptyBody
                , expect = Http.expectWhatever DoNothing
                , tracker = Nothing
                , timeout = Nothing
                }
            )

        DoNothing _ ->
            ( { model | currentPage = Top }, Navigation.pushUrl model.key "/" )

        ChooseFilesClicked ->
            ( model, File.Select.files [ "*/*" ] FilesSelected )

        FilesSelected file files ->
            let
                ( newModel, newCommands ) =
                    uploadTheseFiles (file :: files) model
            in
            ( newModel, newCommands )

        UploadFilesCloseClicked ->
            ( model, Cmd.none )

        ToggleSelection reportId ->
            let
                newSelectedReports =
                    if List.member reportId model.selectedReportIds then
                        List.filter (\rId -> rId /= reportId) model.selectedReportIds

                    else
                        reportId :: model.selectedReportIds
            in
            ( { model | selectedReportIds = newSelectedReports }, Cmd.none )

        DeleteClicked ->
            --  Confirm if they indeed want to delete
            ( { model | showDeleteReportsConfirmation = True }, Cmd.none )

        DeleteReportsConfirmationDeleteClicked ->
            --  Now, go ahead and delete the selected reports
            let
                requestsToDelete : List (Cmd Msg)
                requestsToDelete =
                    List.map
                        (\reportId ->
                            request
                                { method = "DELETE"
                                , headers =
                                    [ header "apiKey" supabaseAnonymousToken
                                    , header "Authorization" ("Bearer " ++ model.supabaseAuthorisedToken)

                                    -- , header "returning" "minimal"
                                    ]
                                , url = supabaseURL ++ "/rest/v1/reports?id=eq." ++ String.fromInt reportId
                                , body = Http.emptyBody
                                , expect = expectWhatever (ReportDeleted reportId)
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                        )
                        model.selectedReportIds
            in
            ( { model | showDeleteReportsConfirmation = False, selectedReportIds = [] }
            , Cmd.batch requestsToDelete
            )

        DeleteReportsConfirmationCloseClicked ->
            ( { model | showDeleteReportsConfirmation = False }, Cmd.none )

        ReportDeleted reportId result ->
            let
                maybeReportWithThisId =
                    List.filter (\r -> r.id == reportId) model.reports
                        |> List.head

                maybeRequestToRemoveFromStorage : Maybe (Cmd Msg)
                maybeRequestToRemoveFromStorage =
                    Maybe.map
                        (\report ->
                            request
                                { method = "DELETE"
                                , headers =
                                    [ header "apiKey" supabaseAnonymousToken
                                    , header "Authorization" ("Bearer " ++ model.supabaseAuthorisedToken)
                                    ]
                                , url = supabaseURL ++ "/storage/v1/object/reports/" ++ Maybe.withDefault "" model.currentUsersId ++ "/" ++ report.name
                                , body = emptyBody
                                , expect = expectWhatever IgnoreSuccessfulHttpRequests
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                        )
                        maybeReportWithThisId
            in
            case result of
                Ok _ ->
                    ( { model | reports = List.filter (\report -> report.id /= reportId) model.reports }
                    , Maybe.withDefault Cmd.none maybeRequestToRemoveFromStorage
                    )

                Err e ->
                    let
                        error =
                            getHttpErrorToDisplay e
                    in
                    ( { model | errors = error :: model.errors }
                    , pauseAndDelistErrors [ error ]
                    )

        DownloadClicked ->
            --  This is where we start the process of downloading selected reports' files
            let
                commands =
                    case model.reports of
                        [] ->
                            Cmd.none

                        _ ->
                            model.reports
                                |> List.filter (\r -> List.member r.id model.selectedReportIds)
                                |> List.map
                                    (\r ->
                                        request
                                            { method = "GET"
                                            , headers =
                                                [ Http.header "apikey" supabaseAnonymousToken
                                                , Http.header "Authorization" ("Bearer " ++ model.supabaseAuthorisedToken)
                                                ]
                                            , url = supabaseURL ++ "/storage/v1/object/authenticated/reports/" ++ Maybe.withDefault "" model.currentUsersId ++ "/" ++ r.name
                                            , body = emptyBody
                                            , expect = expectBytesResponse (DownloadedFile r.id) (resolveResponse Ok)
                                            , timeout = Nothing
                                            , tracker = Nothing
                                            }
                                    )
                                |> Cmd.batch
            in
            ( model, commands )

        DownloadedFile reportId result ->
            let
                ( newModel, newCommand ) =
                    model.reports
                        |> List.filter (\r -> r.id == reportId)
                        |> List.head
                        |> (\maybeReport ->
                                case maybeReport of
                                    Just report ->
                                        case result of
                                            Ok bytes ->
                                                ( model, File.Download.bytes report.name report.mime bytes )

                                            Err err ->
                                                let
                                                    error =
                                                        getHttpErrorToDisplay err
                                                in
                                                ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

                                    Nothing ->
                                        let
                                            error =
                                                "An unexpected error occurred when trying to download the report. Please try again or report the error."
                                        in
                                        ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )
                           )
            in
            ( newModel, newCommand )

        FilesDropped files ->
            let
                ( newModel, newCommands ) =
                    uploadTheseFiles files model
            in
            ( newModel, newCommands )

        GotUploadProgress fileBeingUploadedId progress ->
            let
                newFilesBeingUploaded =
                    List.map
                        (\fbu ->
                            case ( fbu.id == fileBeingUploadedId, progress ) of
                                ( True, Http.Sending values ) ->
                                    { fbu | uploadStatus = Uploading values.sent values.size }

                                _ ->
                                    fbu
                        )
                        model.filesBeingUploaded
            in
            ( { model | filesBeingUploaded = newFilesBeingUploaded }, Cmd.none )

        FileUploaded fileBeingUploaded result ->
            --  Now update the list of files being uploaded
            --  and record the details of the file in the database
            case result of
                Ok _ ->
                    let
                        newFilesBeingUploaded =
                            List.filter (\fbu -> fbu.id /= fileBeingUploaded.id) model.filesBeingUploaded
                    in
                    ( { model | filesBeingUploaded = newFilesBeingUploaded }
                    , Task.perform (InsertUploadedFilesDetails fileBeingUploaded) Time.now
                    )

                Err e ->
                    ( { model
                        | filesBeingUploaded = updateFileBeingUploadedsStatus model.filesBeingUploaded fileBeingUploaded.id <| ErrorWhileUploading <| getHttpErrorToDisplay e
                        , errors = getHttpErrorToDisplay e :: model.errors
                      }
                    , Cmd.none
                    )

        InsertUploadedFilesDetails fileBeingUploaded timestamp ->
            ( model
            , request
                { method = "POST"
                , url = supabaseURL ++ "/rest/v1/reports"
                , headers =
                    [ header "apiKey" supabaseAnonymousToken
                    , header "Authorization" ("Bearer " ++ model.supabaseAuthorisedToken)
                    , header "Prefer" "return=representation" --  This can be "representation" or "minimal"; "representation" returns the row just inserted, and minimal returns absolutely nothing
                    ]
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "name", Encode.string <| File.name fileBeingUploaded.file )
                            , ( "size", Encode.int <| File.size fileBeingUploaded.file )
                            , ( "mime", Encode.string <| File.mime fileBeingUploaded.file )
                            , ( "userId", Encode.string <| Maybe.withDefault "" model.currentUsersId )
                            , ( "uploadedOn", Encode.int <| Time.posixToMillis timestamp )
                            ]
                        )
                , expect = expectJson InsertedUploadedFilesDetails reportsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        InsertedUploadedFilesDetails result ->
            case result of
                Ok reports ->
                    ( { model | reports = List.append model.reports reports }, Cmd.none )

                Err e ->
                    let
                        error =
                            getHttpErrorToDisplay e
                    in
                    ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

        IgnoreSuccessfulHttpRequests result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err e ->
                    let
                        error =
                            getHttpErrorToDisplay e
                    in
                    ( { model | errors = error :: model.errors }, pauseAndDelistErrors [ error ] )

        DelistErrors errorsToDelist ->
            let
                newErrors =
                    Set.diff (Set.fromList model.errors) (Set.fromList errorsToDelist)
                        |> Set.toList
            in
            ( { model | errors = newErrors }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


pauseAndDelistErrors : Errors -> Cmd Msg
pauseAndDelistErrors errors =
    Task.perform (\_ -> DelistErrors errors) (Process.sleep 5000)


resolveResponse : (body -> Result String a) -> Http.Response body -> Result Http.Error a
resolveResponse toResult response =
    case response of
        Http.BadUrl_ url_ ->
            Err (Http.BadUrl url_)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            Result.mapError Http.BadBody (toResult body)


uploadTheseFiles : List File -> Model -> ( Model, Cmd Msg )
uploadTheseFiles files model =
    let
        namesOfFilesAlreadyBeingUploaded : List String
        namesOfFilesAlreadyBeingUploaded =
            model.filesBeingUploaded
                --  Files that had problems when last uploaded
                --  should be allowed to be uplodaded again
                |> List.filter
                    (\fbu ->
                        case fbu.uploadStatus of
                            ErrorWhileUploading _ ->
                                False

                            _ ->
                                True
                    )
                |> List.map (\fbu -> File.name fbu.file)

        namesOfReportsFiles : List String
        namesOfReportsFiles =
            List.map .name model.reports

        fileNamesToCheckAgainst : List String
        fileNamesToCheckAgainst =
            List.concat [ namesOfFilesAlreadyBeingUploaded, namesOfReportsFiles ]

        filesThatCanBeUploaded : List File
        filesThatCanBeUploaded =
            files
                --  Files that have been or are being uploaded will be rejected
                |> List.filter (\file -> not <| List.member (File.name file) fileNamesToCheckAgainst)
                --  Files that are too large are rejected
                |> List.filter (\file -> File.size file < (50 * 1000 * 1000))
                --  Files that have unsafe characters in their names will be rejected
                |> List.filter (\file -> File.name file |> charactersAreAWSSafe)

        idsToUse : List Int
        idsToUse =
            List.range
                model.lastUsedIndexNumberForFilesBeingUploaded
                (model.lastUsedIndexNumberForFilesBeingUploaded + List.length filesThatCanBeUploaded)

        newFilesBeingUploaded : List FileBeingUploaded
        newFilesBeingUploaded =
            List.map2
                (\id file ->
                    { id = id
                    , file = file
                    , uploadStatus = Uploading 0 1
                    }
                )
                idsToUse
                filesThatCanBeUploaded

        commandsToUploadTheFiles : List (Cmd Msg)
        commandsToUploadTheFiles =
            List.map
                (\fileBeingUploaded ->
                    request
                        { method = "POST"
                        , url = supabaseURL ++ "/storage/v1/object/reports/" ++ Maybe.withDefault "" model.currentUsersId ++ "/" ++ File.name fileBeingUploaded.file
                        , headers =
                            [ header "apiKey" supabaseAnonymousToken
                            , header "Authorization" ("Bearer " ++ model.supabaseAuthorisedToken)
                            ]
                        , body = fileBody fileBeingUploaded.file
                        , expect = expectJson (FileUploaded fileBeingUploaded) fileUploadedResponseDecoder
                        , timeout = Nothing
                        , tracker = Just <| String.fromInt fileBeingUploaded.id
                        }
                )
                newFilesBeingUploaded

        errorsAboutFileNamesWithUnsafeCharacters : List String
        errorsAboutFileNamesWithUnsafeCharacters =
            files
                |> List.filter (\file -> File.name file |> charactersAreAWSSafe |> not)
                |> List.map (\file -> "The file name of '" ++ File.name file ++ "' uses characters that aren't safe on the server. Please rename the file and try again.")

        errorsAboutFilesBeingTooLarge : List String
        errorsAboutFilesBeingTooLarge =
            files
                |> List.filter (\file -> File.size file > (50 * 1000 * 1000))
                |> List.map (\file -> "The file '" ++ File.name file ++ "' is too large and won't be uploaded.")

        errorsAboutFilesThatAreAlreadyOnRecord : List String
        errorsAboutFilesThatAreAlreadyOnRecord =
            Set.intersect
                (Set.fromList fileNamesToCheckAgainst)
                (Set.fromList <| List.map File.name files)
                |> Set.map (\fn -> "The file '" ++ fn ++ "' is a duplicate and won't be uploaded.")
                |> Set.toList

        allErrors : List String
        allErrors =
            List.concat [ errorsAboutFileNamesWithUnsafeCharacters, errorsAboutFilesBeingTooLarge, errorsAboutFilesThatAreAlreadyOnRecord ]
    in
    ( { model
        | filesBeingUploaded = List.append model.filesBeingUploaded newFilesBeingUploaded
        , errors = List.concat [ model.errors, allErrors ]
        , lastUsedIndexNumberForFilesBeingUploaded = model.lastUsedIndexNumberForFilesBeingUploaded + List.length filesThatCanBeUploaded
      }
    , Cmd.batch (pauseAndDelistErrors allErrors :: commandsToUploadTheFiles)
    )


updateFileBeingUploadedsStatus : List FileBeingUploaded -> Int -> UploadStatus -> List FileBeingUploaded
updateFileBeingUploadedsStatus filesBeingUploaded id uploadStatus =
    List.map
        (\fbu ->
            if fbu.id == id then
                { fbu | uploadStatus = uploadStatus }

            else
                fbu
        )
        filesBeingUploaded


getHttpErrorToDisplay : Http.Error -> String
getHttpErrorToDisplay err =
    case err of
        Http.BadUrl s ->
            "An error occurred that shouldn't have. Please try again. Additional information: " ++ s

        Http.Timeout ->
            "The task couldn't be accomplished in time. Please try again."

        Http.NetworkError ->
            "You seem to be offline. Please connect back and try again."

        Http.BadStatus code ->
            "A problem occurred. Please try again or report this error. Additional information: Status code " ++ String.fromInt code

        Http.BadBody explanation ->
            "An unexpected problem occurred. Please try again or report this error. Additional information: " ++ explanation


charactersAreAWSSafe : String -> Bool
charactersAreAWSSafe s =
    -- If the string contains any of the below characters, it is unsafe
    [ "\\", "{", "^", "}", "%", "`", "]", "\"", "'", ">", "[", "~", "<", "#", "|", "’" ]
        |> List.map (\c -> String.contains c s |> not)
        |> List.foldl (&&) True


fileUploadedResponseDecoder : Decoder FileUploadedResponse
fileUploadedResponseDecoder =
    Decode.map FileUploadedResponse (field "Key" string)


updateFilesBeingUploaded : List FileBeingUploaded -> Int -> (FileBeingUploaded -> FileBeingUploaded) -> List FileBeingUploaded
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
    Validate.all
        [ Validate.ifBlank .email "Please type in your email"
        , Validate.ifBlank .password "Please type in your password"
        , Validate.ifInvalidEmail .email (\_ -> "Please enter a valid email")
        ]


signUpValidator : Validator String Model
signUpValidator =
    Validate.all
        [ Validate.ifBlank .email "Please type in your email"
        , Validate.ifBlank .password "Please type in your name"
        , Validate.ifInvalidEmail .email (\_ -> "Please enter a valid email")
        , Validate.ifTrue (\model -> String.length model.password < 6) "Please use at least 6 characters for your password"
        ]


signInAUser : String -> String -> Cmd Msg
signInAUser email password =
    Http.request
        { method = "POST"
        , url = supabaseURL ++ "/auth/v1/token?grant_type=password"
        , headers =
            [ header "apikey" supabaseAnonymousToken
            , header "Content-Type" "application/json"
            ]
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
                )
        , expect = Http.expectJson UserSignedIn accessDetailsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


signUpResponseDecoder : Decoder (List String)
signUpResponseDecoder =
    field "identities" identitiesDecoder


identitiesDecoder : Decoder (List String)
identitiesDecoder =
    Decode.list idDecoder


idDecoder : Decoder String
idDecoder =
    field "id" string


accessTokenDecoder : Decoder String
accessTokenDecoder =
    field "access_token" string


userIdDecoder : Decoder String
userIdDecoder =
    Decode.at [ "user", "id" ] string


accessDetailsDecoder : Decoder AccessDetails
accessDetailsDecoder =
    Decode.map2 AccessDetails accessTokenDecoder userIdDecoder


fetchTheUsersUploadedFiles : String -> Cmd Msg
fetchTheUsersUploadedFiles authorisedToken =
    Http.request
        { method = "GET"
        , url = supabaseURL ++ "/rest/v1/reports?select=id,name,size,mime,uploadedOn"
        , headers =
            [ header "apiKey" supabaseAnonymousToken
            , header "Authorization" ("Bearer " ++ authorisedToken)
            ]
        , body = Http.emptyBody
        , expect = Http.expectJson FetchedUploadedFiles reportsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


reportsDecoder : Decode.Decoder (List Report)
reportsDecoder =
    Decode.list reportDecoder


reportDecoder : Decode.Decoder Report
reportDecoder =
    Decode.map5 Report
        (field "id" int)
        (field "name" string)
        (field "size" int)
        (field "mime" string)
        (field "uploadedOn" int |> Decode.map Time.millisToPosix)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentPage of
        Home ->
            model.filesBeingUploaded
                |> List.map (\fbu -> Http.track (String.fromInt fbu.id) (GotUploadProgress fbu.id))
                |> Sub.batch

        _ ->
            Sub.none



--  VIEWS


view : Model -> Document Msg
view model =
    { title = "Bureau"
    , body =
        case urlToPage model.url of
            Top ->
                top

            Login ->
                logIn model

            SignUp ->
                signUp model

            Home ->
                home model

            NotFound ->
                notFound
    }


top : List (Html Msg)
top =
    [ div
        [ class "h-full grid gap-8 auto-cols-min justify-center items-center text-center content-center" ]
        [ div []
            [ div [ class "font-serif text-9xl" ] [ text "Bureau" ]
            , div [ class "text-xl" ] [ div [] [ text "Store your health records online for free. Access them anytime." ] ]
            ]
        , a [ href "/signup", class "btn w-full" ] [ text "Sign up or Log in" ]
        ]
    ]


notFound : List (Html Msg)
notFound =
    [ h1 [] [ text "Not found" ], div [] [ text "We couldn't find a page like that" ] ]


logIn : Model -> List (Html Msg)
logIn model =
    [ div
        [ class "w-full h-full grid justify-center content-center" ]
        [ div
            [ cardClasses ]
            (List.concat
                [ [ div
                        [ class "card-body" ]
                        [ h2 [ class "card-title" ] [ text "Log in" ]
                        , input [ type_ "email", required True, placeholder "Your email address", class "input input-bordered", onInput EmailTyped, value model.email ] []
                        , input [ type_ "password", required True, placeholder "Your password", class "input input-bordered", onInput PasswordTyped, value model.password ] []
                        , div
                            [ class "card-actions justify-between items-center" ]
                            [ a [ href "/signup" ] [ text "Sign up" ]
                            , button [ onClick LogInClicked, class "btn btn-primary" ] [ text "Log in" ]
                            ]
                        ]
                  ]
                , if List.length model.errors == 0 then
                    []

                  else
                    [ errorsDisplay model.errors ]
                ]
            )
        ]
    ]


signUp : Model -> List (Html Msg)
signUp model =
    [ div
        [ class "w-full h-full grid justify-center content-center" ]
        [ div
            [ cardClasses ]
            [ div
                [ class "card-body" ]
                [ h2 [ class "card-title" ] [ text "Sign up" ]
                , div
                    [ class "w-full grid grid-cols-1 gap-2" ]
                    (case model.signUpStatus of
                        SignUpStatusDefault ->
                            signUpDefault model

                        SignUpStatusConfirmationRequired ->
                            signUpConfirmationRequired
                    )
                ]
            , errorsDisplay model.errors
            ]
        ]
    ]


signUpDefault : Model -> List (Html Msg)
signUpDefault model =
    [ input [ type_ "email", required True, placeholder "Your email address", class "input input-bordered", onInput EmailTyped, value model.email ] []
    , input [ type_ "password", required True, placeholder "Set a password", class "input input-bordered", onInput PasswordTyped, value model.password ] []
    , div
        [ class "card-actions justify-between items-center" ]
        [ a [ href "/login" ] [ text "Log in" ]
        , button [ onClick SignUpClicked, class "btn btn-primary" ] [ text "Sign up" ]
        ]
    ]


signUpConfirmationRequired : List (Html Msg)
signUpConfirmationRequired =
    [ div [ class "w-fit" ] [ text "We've sent you a confirmation email. Please click the confirmation link and then try logging in." ]
    , div
        [ class "card-actions justify-end" ]
        [ a [ class "btn btn-primary", href "/login" ] [ text "Log in" ] ]
    ]


home : Model -> List (Html Msg)
home model =
    if model.supabaseAuthorisedToken == "" then
        --  The user's not logged in
        [ div
            [ class "w-full h-full grid justify-center content-center" ]
            [ div
                [ class "card shadow-xl w-96" ]
                [ div [ class "card-body" ]
                    [ div [ class "card-title" ] [ text "Not logged in" ]
                    , text "We can't show you anything because you aren't logged in."
                    , div
                        [ class "card-actions justify-end" ]
                        [ button [ class "btn btn-primary", onClick GoToLoginPageClicked ] [ text "Go to Log In page" ]
                        ]
                    ]
                ]
            ]
        ]

    else
        --  Show the user their files
        List.concat
            [ [ div
                    [ class "w-full pb-8 px-8 grid grid-cols-1 gap-4 justify-start content-start card"
                    ]
                    [ div
                        [ class "card-body" ]
                        [ div
                            [ class "card-actions justify-between items-center" ]
                            [ div [] [ text model.email ]
                            , button [ class "btn btn-ghost", onClick LogOutClicked ] [ text "Log out" ]
                            ]
                        , reportsDisplay model.filesBeingUploaded model.reports model.currentTimezone model.selectedReportIds
                        ]
                    ]
              ]
            , if List.length model.errors > 0 then
                [ errorsDisplay model.errors ]

              else
                []
            , if model.showDeleteReportsConfirmation then
                [ deleteReportsConfirmation model.reports model.selectedReportIds ]

              else
                []
            ]


fileBeingUploadedDisplay : FileBeingUploaded -> Html msg
fileBeingUploadedDisplay fileBeingUploaded =
    div
        [ class "fileBeingUploaded"
        , class "w-full p-4 grid gap-x-4 rows-1 items-center text-sm"
        ]
        [ span [ class "overflow-y-auto" ] [ text <| File.name fileBeingUploaded.file ]
        , span
            [ class "overflow-y-auto" ]
            [ text <| getDisplayFileSize fileBeingUploaded.file ]
        , case fileBeingUploaded.uploadStatus of
            Uploading sent size ->
                progress [ class "progress w-full", value <| String.fromInt sent, Html.Attributes.max <| String.fromInt size ] []

            Uploaded ->
                progress [ class "progress w-full", value "1", Html.Attributes.max "1" ] []

            ErrorWhileUploading error ->
                span [] [ text error ]
        ]


getDisplayFileSize : File -> String
getDisplayFileSize file =
    let
        fileSize =
            File.size file
                |> Basics.toFloat
    in
    [ ( 1000 ^ 0, "B" ), ( 1000 ^ 1, "kB" ), ( 1000 ^ 2, "MB" ), ( 1000 ^ 3, "GB" ), ( 1000 ^ 4, "TB" ), ( 1000 ^ 5, "PB" ) ]
        |> List.map (\( divisor, unit ) -> ( fileSize / Basics.toFloat divisor, unit ))
        |> List.filter (\( value, _ ) -> value >= 1 && value <= 1000)
        |> List.head
        |> (\maybeHead ->
                case maybeHead of
                    Just ( value, unit ) ->
                        value
                            |> Basics.round
                            |> String.fromInt
                            |> (\x -> x ++ unit)

                    Nothing ->
                        ""
           )


deleteReportsConfirmation : List Report -> List ReportId -> Html Msg
deleteReportsConfirmation reports selectedReportIds =
    let
        selectedReportsFileNames =
            List.filter (\r -> List.member r.id selectedReportIds) reports
                |> List.sortBy .id
                |> List.reverse
                |> List.map .name
    in
    div
        [ class "modal modal-open" ]
        [ div
            [ class "modal-box max-h-full" ]
            [ h3 [ class "text-lg font-bold pb-4" ] [ text "Confirm deletion" ]
            , p [ class "pb-4" ] [ text "Please confirm that you want to delete the following reports. This action cannot be undone." ]
            , div
                [ class "max-h-full overflow-y-auto w-full bg-slate-100 rounded-md p-8" ]
                [ ol [] (List.map (\n -> li [ class "pb-2" ] [ text n ]) selectedReportsFileNames) ]
            , div
                [ class "modal-action" ]
                [ button [ class "btn", onClick DeleteReportsConfirmationCloseClicked ] [ text "Close" ]
                , button [ class "btn btn-primary", onClick DeleteReportsConfirmationDeleteClicked ] [ text "Delete" ]
                ]
            ]
        ]


errorsDisplay : Errors -> Html Msg
errorsDisplay errors =
    div
        [ class "fixed bottom-0 left-0 p-2 grid grid-cols-1 auto-rows-auto gap-1"
        ]
        (List.map errorDisplay errors)


errorDisplay : String -> Html Msg
errorDisplay error =
    div [ class "alert alert-error shadow-lg transition-opacity" ] [ text error ]


getUploadStatus : FileBeingUploaded -> String
getUploadStatus fileToProcess =
    case fileToProcess.uploadStatus of
        Uploading sent size ->
            String.fromInt (sent // size) ++ "% uploaded"

        Uploaded ->
            "Uploaded"

        ErrorWhileUploading _ ->
            "Problem uploading"


reportsDisplay : List FileBeingUploaded -> List Report -> Zone -> List ReportId -> Html Msg
reportsDisplay filesBeingUploaded reports zone selectedReportIds =
    let
        sortedReports =
            reports
                |> List.sortWith comparePosix
                |> List.reverse
    in
    div
        [ cardClasses
        ]
        [ div
            [ class "card-body" ]
            [ div [ class "card-title" ] [ text "Your reports" ]
            , div
                [ class "rounded-md bg-slate-100 border border-slate-300 p-4 grid grid-cols-1 auto-rows-min" ]
                (List.concat
                    [ [ dropZone ]
                    , case filesBeingUploaded of
                        [] ->
                            []

                        _ ->
                            let
                                values : List ( Int, Int )
                                values =
                                    List.map
                                        (\fbu ->
                                            case fbu.uploadStatus of
                                                Uploading sent size ->
                                                    ( sent, size )

                                                _ ->
                                                    ( 0, 0 )
                                        )
                                        filesBeingUploaded

                                totalSent =
                                    List.foldl (\( sent, _ ) total -> total + sent) 0 values

                                totalSize =
                                    List.foldl (\( _, size ) total -> total + size) 0 values
                            in
                            [ progress
                                [ class "progress w-full"
                                , Html.Attributes.value <| String.fromInt totalSent
                                , Html.Attributes.max <| String.fromInt totalSize
                                ]
                                []
                            ]
                    ]
                )
            , div
                [ class "grid w-full" ]
                (List.indexedMap (reportDisplay zone selectedReportIds) sortedReports)
            , div
                [ class "card-actions justify-end" ]
                [ button [ class "btn", disabled (List.length selectedReportIds == 0), onClick DeleteClicked ] [ text "Delete" ]
                , button [ class "btn", disabled (List.length selectedReportIds == 0), onClick DownloadClicked ] [ text "Download" ]
                ]
            ]
        ]


dropZone : Html Msg
dropZone =
    div
        [ class "w-full md:p-4 grid justify-items-center"
        , onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        [ span [ class "text-slate-400 text-center hidden md:block" ] [ text "Drop files here or click the button" ]
        , button [ class "btn btn-ghost", onClick ChooseFilesClicked ] [ text "Choose files" ]
        ]


reportDisplay : Zone -> List ReportId -> Int -> Report -> Html Msg
reportDisplay zone selectedReportIds index report =
    div
        [ class "reportDisplay"
        , class "grid gap-4 rounded-md p-4 active:bg-slate-300"
        , class "hover:bg-slate-100"
        , classList [ ( "bg-slate-200", List.member report.id selectedReportIds ) ]
        , onClick (ToggleSelection report.id)
        ]
        [ div [ class "text-right" ] [ index + 1 |> String.fromInt |> text ]
        , div
            [ class "overflow-hidden" ]
            [ report.uploadedOn
                |> getDisplayDate zone
                |> text
            ]
        , div [ class "col-start-2 sm:col-start-3 overflow-x-auto text-overflow-ellipsis" ] [ text report.name ]
        ]



-- HELPER FUNCTIONS


comparePosix : Report -> Report -> Order
comparePosix a b =
    compare (Time.posixToMillis a.uploadedOn) (Time.posixToMillis b.uploadedOn)


cardClasses : Attribute msg
cardClasses =
    class "card bg-base-100 shadow-xl border border-slate-100"


getDisplayDate : Zone -> Posix -> String
getDisplayDate zone time =
    String.join " "
        [ Time.toYear zone time |> String.fromInt
        , Time.toMonth zone time |> getMonthName
        , Time.toDay zone time |> String.fromInt
        , if zone == Time.utc then
            " UTC"

          else
            ""
        ]


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    on "dragstart" (Decode.succeed msg)


onDragOver : Msg -> Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" (Decode.succeed ( msg, True ))


onDrop : Msg -> Attribute Msg
onDrop msg =
    preventDefaultOn "drop" (Decode.succeed ( msg, True ))


onFilesDrop : (List File -> Msg) -> Attribute Msg
onFilesDrop msg =
    preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))


filesDecoder : Decoder (List File)
filesDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.list File.decoder)


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" (Decode.succeed msg)


getMonthName : Time.Month -> String
getMonthName month =
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
