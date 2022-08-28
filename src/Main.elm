module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import File exposing (File)
import File.Select
import Html exposing (Attribute, Html, a, button, div, h1, h2, h3, input, li, ol, p, text)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, required, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Http exposing (emptyBody, expectJson, expectWhatever, fileBody, header, request)
import Json.Decode as Decode exposing (Decoder, Error(..), field, int, string)
import Json.Encode as Encode
import Process
import Set exposing (Set)
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
    , loginErrors : Errors
    , signUpErrors : Errors
    , currentUsersId : Maybe String

    -- Home stuff
    , selectedReportIds : List ReportId
    , showDeleteReportsConfirmation : Bool

    -- User's data
    , reports : List Report
    , errors : Errors

    -- Files being uploaded
    , lastUsedIndexNumberForFilesBeingUploaded : Int
    , filesBeingUploaded : FilesBeingUploaded
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


type alias SessionDetails =
    { token : String
    , userId : String
    }


type alias Errors =
    List String


type alias FileBeingUploaded =
    { id : Int
    , file : File
    , uploadStatus : UploadStatus
    }


type UploadStatus
    = Uploading Float
    | Uploaded
    | ErrorWhileUploading String


type alias FilesBeingUploaded =
    List FileBeingUploaded


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


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { url = url
      , key = key
      , currentTimezone = Time.utc
      , supabaseAuthorisedToken = ""
      , currentPage = urlToPage url
      , email = "azurewaters@gmail.com"
      , password = "QcGmuuQgpEKpnsSpIqir"
      , loginErrors = []
      , signUpErrors = []
      , currentUsersId = Nothing
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
    | DelistErrors
      --  Login and registration messages
    | EmailTyped String
    | PasswordTyped String
    | LogInClicked
    | SignUpClicked
    | UserSignedIn (Result Http.Error AccessDetails)
    | FetchedUploadedFiles (Result Http.Error (List Report))
    | UserSignedUp (Result Http.Error String)
      -- Home messages
    | GoToLoginPageClicked
    | LogOutClicked
    | UploadClicked
    | FilesSelected File (List File)
    | ToggleSelection ReportId
    | DeleteClicked
    | DeleteReportsConfirmationDeleteClicked
    | DeleteReportsConfirmationCloseClicked
    | ReportDeleted ReportId (Result Http.Error ())
      --  File upload messages
    | FilesDropped (List File)
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
                    ( { model | loginErrors = [] }, signInAUser model.email model.password )

                Err errors ->
                    ( { model | loginErrors = errors }, Cmd.none )

        SignUpClicked ->
            case Validate.validate signUpValidator model of
                Ok _ ->
                    ( { model | signUpErrors = [] }
                    , signUpAUser model.email model.password
                    )

                Err errors ->
                    ( { model | signUpErrors = errors }, Cmd.none )

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
                    ( { model | loginErrors = "Unable to log in" :: model.loginErrors }, Cmd.none )

        FetchedUploadedFiles value ->
            case value of
                Ok result ->
                    ( { model | reports = result }, Cmd.none )

                Err err ->
                    ( Debug.log (Debug.toString err) model, Cmd.none )

        UserSignedUp value ->
            ( Debug.log (Debug.toString value) model, Cmd.none )

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

        UploadClicked ->
            ( model, File.Select.files [ "image/jpg", "image/jpeg", "image/png", "application/pdf" ] FilesSelected )

        FilesSelected file files ->
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
                    List.map (makeDeleteReportRequest model.supabaseAuthorisedToken) model.selectedReportIds
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

                Err error ->
                    ( { model | errors = ("Report " ++ String.fromInt reportId ++ " could not be uploaded. The error reported was " ++ Debug.log (Debug.toString error) "") :: model.errors }
                    , Cmd.none
                    )

        FilesDropped files ->
            let
                imageFiles : List File
                imageFiles =
                    List.filter itIsAnImage files

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

                uniqueImageFiles : List File
                uniqueImageFiles =
                    List.filter
                        (\file -> not <| List.member (File.name file) fileNamesToCheckAgainst)
                        imageFiles

                idsToUse : List Int
                idsToUse =
                    List.range model.lastUsedIndexNumberForFilesBeingUploaded (model.lastUsedIndexNumberForFilesBeingUploaded + List.length uniqueImageFiles)

                newFilesBeingUploaded : FilesBeingUploaded
                newFilesBeingUploaded =
                    List.map2
                        (\id file ->
                            { id = id
                            , file = file
                            , uploadStatus = Uploading 0
                            }
                        )
                        idsToUse
                        uniqueImageFiles

                commandsToUploadTheFiles : List (Cmd Msg)
                commandsToUploadTheFiles =
                    List.map
                        (makeFileUploadRequest model.supabaseAuthorisedToken (Maybe.withDefault "" model.currentUsersId))
                        newFilesBeingUploaded

                -- Now find the errors
                --  First, the files that aren't images
                --  Second, the files that are already being uploaded
                fileNames : Set String
                fileNames =
                    List.map File.name files
                        |> Set.fromList

                errorsAboutFilesThatArentImages : List String
                errorsAboutFilesThatArentImages =
                    let
                        imageFileNames =
                            Set.fromList <| List.map File.name imageFiles
                    in
                    Set.diff fileNames imageFileNames
                        |> Set.map (\fn -> "The file '" ++ fn ++ "' is not an image and won't be uploaded.")
                        |> Set.toList

                errorsAboutFilesThatAreAlreadyOnRecord : List String
                errorsAboutFilesThatAreAlreadyOnRecord =
                    Set.intersect (Set.fromList fileNamesToCheckAgainst) fileNames
                        |> Set.map (\fn -> "The file '" ++ fn ++ "' is a duplicate and won't be uploaded.")
                        |> Set.toList
            in
            ( { model
                | filesBeingUploaded = List.append model.filesBeingUploaded newFilesBeingUploaded
                , errors = List.concat [ model.errors, errorsAboutFilesThatArentImages, errorsAboutFilesThatAreAlreadyOnRecord ]
                , lastUsedIndexNumberForFilesBeingUploaded = model.lastUsedIndexNumberForFilesBeingUploaded + List.length uniqueImageFiles
              }
            , Cmd.batch <|
                List.append
                    commandsToUploadTheFiles
                    [ Process.sleep 3000 |> Task.perform (\_ -> DelistErrors) ]
            )

        FileUploaded fileBeingUploaded result ->
            --  Now update the list of files being uploaded
            --  and record the details of the file in the database
            case result of
                Ok _ ->
                    ( { model | filesBeingUploaded = List.filter (\fbu -> fbu.id /= fileBeingUploaded.id) model.filesBeingUploaded }
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
                    ( Debug.log ("InsertedUploadedFilesDetails - " ++ Debug.toString e) model, Cmd.none )

        IgnoreSuccessfulHttpRequests result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err e ->
                    ( Debug.log (Debug.toString e) model, Cmd.none )

        DelistErrors ->
            ( { model | errors = [] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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


makeDeleteReportRequest : String -> ReportId -> Cmd Msg
makeDeleteReportRequest supabaseAuthorisedToken reportId =
    request
        { method = "DELETE"
        , headers =
            [ header "apiKey" supabaseAnonymousToken
            , header "Authorization" ("Bearer " ++ supabaseAuthorisedToken)

            -- , header "returning" "minimal"
            ]
        , url = supabaseURL ++ "/rest/v1/reports?id=eq." ++ String.fromInt reportId
        , body = Http.emptyBody
        , expect = expectWhatever (ReportDeleted reportId)
        , timeout = Nothing
        , tracker = Nothing
        }


itIsAnImage : File -> Bool
itIsAnImage =
    \file -> List.member (File.mime file) [ "image/jpg", "image/jpeg", "image/png", "application/pdf" ]


checkIfCharactersAreAWSSafe : String -> Bool
checkIfCharactersAreAWSSafe s =
    -- Unsafe character names have to be checked
    [ "\\", "{", "^", "}", "%", "`", "]", "\"", "'", ">", "[", "~", "<", "#", "|" ]
        |> List.map (\c -> String.contains c s)
        |> List.foldl (&&) True


makeFileUploadRequest : String -> String -> FileBeingUploaded -> Cmd Msg
makeFileUploadRequest supabaseAuthorisedToken userId fileBeingUploaded =
    request
        { method = "POST"
        , url = supabaseURL ++ "/storage/v1/object/reports/" ++ userId ++ "/" ++ File.name fileBeingUploaded.file
        , headers =
            [ header "apiKey" supabaseAnonymousToken
            , header "Authorization" ("Bearer " ++ supabaseAuthorisedToken)
            ]
        , body = fileBody fileBeingUploaded.file
        , expect = expectJson (FileUploaded fileBeingUploaded) fileUploadedResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fileUploadedResponseDecoder : Decoder FileUploadedResponse
fileUploadedResponseDecoder =
    Decode.map FileUploadedResponse (field "Key" string)


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


signUpAUser : String -> String -> Cmd Msg
signUpAUser email password =
    Http.request
        { method = "POST"
        , url = supabaseURL ++ "/auth/v1/signup"
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
        , expect = Http.expectJson UserSignedUp accessTokenDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


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
subscriptions _ =
    Sub.batch []



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
    [ h1 [] [ text "Hello" ], div [] [ text "This is the company's page" ] ]


notFound : List (Html Msg)
notFound =
    [ h1 [] [ text "Not found" ], div [] [ text "We couldn't find a page like that" ] ]


logIn : Model -> List (Html Msg)
logIn model =
    [ div
        [ class "w-full h-full grid justify-center content-center" ]
        [ div
            [ cardClasses ]
            [ div
                [ class "card-body" ]
                [ h2 [ class "card-title" ] [ text "Log in" ]
                , input [ type_ "email", required True, placeholder "Your email address", class "input input-bordered", onInput EmailTyped, value model.email ] []
                , input [ type_ "password", required True, placeholder "Your password", class "input input-bordered", onInput PasswordTyped, value model.password ] []
                , div [ class "card-actions justify-between items-center" ]
                    [ a [ href "/signup" ] [ text "Sign up" ]
                    , button [ onClick LogInClicked, class "btn btn-primary" ] [ text "Log in" ]
                    ]
                , div [] (List.map (\error -> div [] [ text error ]) model.loginErrors)
                ]
            ]
        ]
    ]


signUp : Model -> List (Html Msg)
signUp model =
    [ div
        [ class "w-full h-full grid justify-center content-center" ]
        [ div
            [ cardClasses ]
            [ div [ class "card-body" ]
                [ h2 [ class "card-title" ] [ text "Sign up" ]
                , input [ type_ "email", required True, placeholder "Your email address", class "input input-bordered", onInput EmailTyped, value model.email ] []
                , input [ type_ "password", required True, placeholder "Set a password", class "input input-bordered", onInput PasswordTyped, value model.password ] []
                , div
                    [ class "card-actions justify-between items-center" ]
                    [ a [ href "/login" ] [ text "Log in" ]
                    , button [ onClick SignUpClicked, class "btn btn-primary" ] [ text "Sign up" ]
                    ]
                ]
            , div [ class "m-4" ] (List.map (\error -> div [] [ text error ]) model.signUpErrors)
            ]
        ]
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
        let
            sortedFiles =
                List.sortWith comparePosix model.reports
                    |> List.reverse
        in
        List.concat
            [ [ div
                    [ class "w-full pb-8 px-8 grid grid-cols-1 gap-4 justify-start content-start card"
                    ]
                    [ div
                        [ class "card-body" ]
                        [ div
                            [ class "card-actions justify-between" ]
                            [ div [] [ text model.email ]
                            , button [ class "btn btn-ghost", onClick LogOutClicked ] [ text "Log out" ]
                            ]
                        , reportsDisplay sortedFiles model.currentTimezone model.selectedReportIds
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
            [ class "modal-box" ]
            [ h3 [ class "text-lg font-bold" ] [ text "Confirm Deletion" ]
            , p [] [ text "Please confirm that you want to delete the following reports:" ]
            , div [ class "max-h-20" ] [ ol [] (List.map (\n -> li [] [ text n ]) selectedReportsFileNames) ]
            , p [] [ text "This action cannot be undone." ]
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
        Uploading progress ->
            (round progress |> String.fromInt) ++ "% uploaded"

        Uploaded ->
            "Uploaded"

        ErrorWhileUploading _ ->
            "Problem uploading"


reportsDisplay : List Report -> Zone -> List ReportId -> Html Msg
reportsDisplay reports zone selectedReportIds =
    div
        [ cardClasses
        , onFilesDrop FilesDropped
        , onDragOver NoOp
        ]
        [ div
            [ class "card-body" ]
            [ div [ class "card-title" ] [ text "Your reports" ]
            , div
                [ class "grid w-full" ]
                (List.indexedMap (reportDisplay zone selectedReportIds) reports)
            , div
                [ class "card-actions justify-end" ]
                [ button [ class "btn", disabled (List.length selectedReportIds == 0), onClick DeleteClicked ] [ text "Delete" ]
                , button [ class "btn", onClick UploadClicked ] [ text "Upload" ]
                ]
            ]
        ]


reportDisplay : Zone -> List ReportId -> Int -> Report -> Html Msg
reportDisplay zone selectedReportIds index report =
    div
        [ class "reportDisplay"
        , class "w-full grid gap-4 rounded-md p-4 active:bg-slate-300"
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
        , div [ class "overflow-hidden" ] [ text report.name ]
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
