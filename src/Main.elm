module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, text)
import Html.Events exposing (onInput)



{-
   TODO:
       - [x] Better Modeling for User and Validation
       - [x] Validate: name and last name, age, and email
       - [ ] Collect all error messages (do not short-circuit on first error)
          - Idea: Use a dict to collect the errors?
-}


type ValidationError
    = EmptyName
    | InvalidAge
    | InvalidEmail String


type alias User =
    { name : String
    , age : Int
    , email : String
    }


type alias Model =
    { name : String
    , age : String
    , email : String
    }


type Msg
    = EnteredNameInput String
    | EnteredAgeInput String
    | EnteredEmailInput String


init : Model
init =
    Model "" "" ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredNameInput name ->
            { model | name = name }

        EnteredAgeInput age ->
            { model | age = age }

        EnteredEmailInput email ->
            { model | email = email }


validateName : User -> Result ValidationError User
validateName user =
    case user.name of
        "" ->
            Err EmptyName

        _ ->
            Ok user


validateAge : User -> Result ValidationError User
validateAge user =
    if user.age < 21 then
        Err InvalidAge

    else
        Ok user


validateEmail : User -> Result ValidationError User
validateEmail user =
    if String.contains "@" user.email then
        Ok user

    else
        Err <| InvalidEmail user.email


parseUser : Model -> Result ValidationError User
parseUser { name, age, email } =
    let
        intAge =
            age
                |> String.toInt
                |> Maybe.withDefault 0
    in
    User name intAge email
        |> Ok


uppercaseName : User -> User
uppercaseName model =
    { model | name = String.toUpper model.name }


validate : Model -> Result ValidationError User
validate model =
    parseUser model
        |> Result.andThen validateName
        -- two-track function
        |> Result.andThen validateAge
        |> Result.andThen validateEmail
        -- One track function converted to two-track
        |> Result.map uppercaseName


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "Name" ]
            , input [ onInput EnteredNameInput ] []
            ]
        , div []
            [ label [] [ text "Age" ]
            , input [ onInput EnteredAgeInput ] []
            ]
        , div []
            [ label [] [ text "Email" ]
            , input [ onInput EnteredEmailInput ] []
            ]
        , viewValidationInfo model
        ]


viewValidationInfo : Model -> Html msg
viewValidationInfo model =
    case validate model of
        Ok _ ->
            div [] [ text "Awesome!, all is good" ]

        Err EmptyName ->
            div [] [ text "Please provide a name" ]

        Err InvalidAge ->
            div [] [ text "You are not old enough :(" ]

        Err (InvalidEmail email) ->
            div [] [ text <| "The provided email is not valid: " ++ email ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }
