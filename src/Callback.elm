module Callback exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)


type Msg
    = CallBack
    | AccountNumber String
    | PhoneNumber String
    | CallBackTime String
    | NotifyResponse (Result Http.Error CBResult)



--MODEL--


type alias Model =
    { accountNumber : String
    , phoneNumber : String
    , callBackTime : String
    , cbStatus : String
    }


type alias CBResult =
    { result : String
    }


initState : Model
initState =
    Model "" "" "" ""


cburl : String
cburl =
    "http://10.78.92.86:9080/callback"



--View --


callbackView : Model -> Html Msg
callbackView model =
    div []
        [ table []
            [ tr []
                [ td []
                    [ label []
                        [ text "Account Number : " ]
                    ]
                , td [] [ input [ placeholder "Account Number", onInput AccountNumber ] [ text model.accountNumber ] ]
                ]
            , tr []
                [ td []
                    [ label []
                        [ text "Phone Number : " ]
                    ]
                , td [] [ input [ placeholder "Phone Number", onInput PhoneNumber ] [ text model.phoneNumber ] ]
                ]
            , tr []
                [ td []
                    [ label []
                        [ text "Callback Time (hh:mm) : " ]
                    ]
                , td [] [ input [ placeholder "Callback Time (hh:mm)", onInput CallBackTime ] [ text model.callBackTime ] ]
                ]
            , tr []
                [ td []
                    []
                , td []
                    [ button [ onClick CallBack ] [ text "Schedule Callback" ]
                    ]
                ]
            ]
        , div []
            [ text model.cbStatus
            ]
        ]



-- Update --


body : Model -> String
body model =
    "accountNumber=" ++ model.accountNumber ++ "&phoneNumber=" ++ model.phoneNumber ++ "&callbackTime=" ++ model.callBackTime


cbResultDecoder : Decoder CBResult
cbResultDecoder =
    Json.map CBResult
        (field "result" string)


notifyCallback : Model -> Cmd Msg
notifyCallback model =
    Http.send NotifyResponse (Http.post cburl (Http.stringBody "application/x-www-form-urlencoded" (body model)) cbResultDecoder)


httpErrToString : Http.Error -> String
httpErrToString err =
    case err of
        Timeout ->
            "Timeout Error"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            "Bad Status Code in Response : " ++ toString response.status.code

        BadPayload msg _ ->
            "Bad Response Payload : " ++ msg

        BadUrl _ ->
            "Invalid URL"


callbackUpdate : Msg -> Model -> ( Model, Cmd Msg )
callbackUpdate msg model =
    case msg of
        AccountNumber an ->
            ( { model | accountNumber = an }, Cmd.none )

        PhoneNumber ph ->
            ( { model | phoneNumber = ph }, Cmd.none )

        CallBackTime cbt ->
            ( { model | callBackTime = cbt }, Cmd.none )

        CallBack ->
            ( model, notifyCallback model )

        NotifyResponse (Ok _) ->
            ( { model | cbStatus = "Successfully scheduled the callback ! " }, Cmd.none )

        NotifyResponse (Err errMsg) ->
            ( { model | cbStatus = "Failed to schedule callback. " ++ httpErrToString errMsg }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program { init = ( initState, Cmd.none ), view = callbackView, update = callbackUpdate, subscriptions = subscriptions }
