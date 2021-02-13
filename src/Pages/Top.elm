module Pages.Top exposing (Model, Msg, Params, page)

import Browser.Navigation exposing (Key)
import Css.Global
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, style, type_, value)
import Html.Styled.Events exposing (..)
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Tailwind.Breakpoints exposing (..)
import Tailwind.Utilities exposing (..)


page : Page Params Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { boxes : Dict Int String
    , selected : String
    }


init : Url Params -> ( Model, Cmd Msg )
init _ =
    ( { boxes = initBoxes "white"
      , selected = "purple"
      }
    , Cmd.none
    )


initBoxes color =
    List.range 0 400 |> List.map (\index -> ( index, color )) |> Dict.fromList



-- UPDATE


type Msg
    = ClickedBox Int
    | ClickedDelete
    | ClickedCreate
    | ClickedFill
    | SelectedColor String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFill ->
            ( { model | boxes = initBoxes model.selected }, Cmd.none )

        SelectedColor color ->
            ( { model | selected = color }, Cmd.none )

        ClickedCreate ->
            ( { model | boxes = initBoxes "white" }, Cmd.none )

        ClickedDelete ->
            ( { model | boxes = Dict.empty }, Cmd.none )

        ClickedBox index ->
            let
                color =
                    case Dict.get index model.boxes of
                        Just "white" ->
                            model.selected

                        _ ->
                            "white"
            in
            ( { model | boxes = Dict.insert index color model.boxes }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Top"
    , body =
        [ body model
        ]
    }


body model =
    div []
        [ Css.Global.global globalStyles
        , p
            [ css
                [ flex
                , uppercase
                , text_gray_700
                , text_sm
                , p_3
                , font_bold
                , bg_gray_100
                , border_b
                , border_gray_300
                ]
            ]
            [ text "Delete and create the boxes, by Paul" ]
        , div
            [ css
                [ w_full
                , flex
                , items_center
                , p_4
                , justify_between
                , h_20
                ]
            ]
            [ div []
                [ button
                    [ css [ mr_2, px_4, py_2, bg_purple_200, text_purple_700, font_bold, border_purple_800, rounded_lg ]
                    , onClick ClickedCreate
                    ]
                    [ text "New" ]
                , button
                    [ css [ mr_2, px_4, py_2, bg_yellow_200, text_yellow_700, font_bold, border_yellow_800, rounded_lg ]
                    , onClick ClickedFill
                    ]
                    [ text "Fill All" ]
                , button
                    [ css [ mr_2, px_4, py_2, bg_red_200, text_red_700, font_bold, border_red_800, rounded_lg ]
                    , onClick ClickedDelete
                    ]
                    [ text "Delete" ]
                ]
            , div
                [ css
                    [ flex
                    , items_center
                    , uppercase
                    , text_gray_500
                    , font_bold
                    , text_xs
                    ]
                ]
                [ p [] [ text "Color Picker" ]
                , viewColorPicker model
                ]
            ]
        , div
            [ css
                [ p_4
                ]
            ]
            [ div
                [ css
                    [ w_full
                    , flex
                    , flex_wrap
                    ]
                ]
                (model.boxes
                    |> Dict.map viewBox
                    |> Dict.values
                )
            ]
        ]


viewBox index color =
    div
        [ css
            [ w_10
            , cursor_pointer
            , h_10
            , border
            , border_gray_200
            ]
        , onClick (ClickedBox index)
        , style "background-color" color
        ]
        []


viewColorPicker model =
    input
        [ css
            [ h_10
            , w_10
            , p_2
            , rounded
            , m_2
            , border_none
            , cursor_pointer
            ]
        , type_ "color"
        , style "background-color" model.selected
        , value model.selected
        , onInput SelectedColor
        ]
        []
