module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Events exposing (onClick, onInput)



{-
   TODO:
       - Better Modeling for User and Validation
       - Validate: name and last name, age, and email
       - Collect all error messages (do not short-circuit on first error)
-}


type Validation
    = NotChecked
    | GotErrors (List String)
    | Succeded


type alias Model =
    { name : String, age : String, validation : Validation }


type Msg
    = EnteredNameInput String
    | EnteredAgeInput String
    | ClickedValidationButton


init : Model
init =
    Model "" "" NotChecked


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredNameInput name ->
            { model | name = name }

        EnteredAgeInput age ->
            { model | age = age }

        ClickedValidationButton ->
            validate model


validateName : a -> Validation
validateName _ =
    Succeded


validateAge : a -> Validation
validateAge _ =
    Succeded


validate : Model -> Model
validate model =
    { model | validation = Succeded }


view : Model -> Html Msg
view { validation } =
    div []
        [ div []
            [ label [] [ text "Name" ]
            , input [ onInput EnteredNameInput ] []
            ]
        , div []
            [ label [] [ text "Age" ]
            , input [ onInput EnteredAgeInput ] []
            ]
        , viewButton
        , viewValidationInfo validation
        ]


viewButton : Html Msg
viewButton =
    div [] [ button [ onClick ClickedValidationButton ] [ text "Validate" ] ]


viewValidationInfo : Validation -> Html msg
viewValidationInfo validation =
    case validation of
        NotChecked ->
            div [] [ text "Please click to validate" ]

        Succeded ->
            div [] [ text "Awesome!, all is good" ]

        GotErrors _ ->
            div [] [ text "Got some errors :(" ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }
