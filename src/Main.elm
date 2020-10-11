module Main exposing (..)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias BlogImage =
    { alt : String
    , src : String
    }


type alias BlogTextContent =
    { text : String
    , image : BlogImage
    , collapsed : Bool
    }


type alias BlogPost =
    { date : String
    , title : String
    , textContent : BlogTextContent
    }


type alias Model =
    { posts : List BlogPost
    }


init : Model
init =
    Model [ samplePost ]



-- UPDATE


type Msg
    = AddPost BlogPost
    | ToggleCollapse Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPost post ->
            let
                oldPosts =
                    model.posts

                newPosts =
                    oldPosts ++ [ post ]
            in
            { model | posts = newPosts }

        ToggleCollapse idx ->
            let
                postArray =
                    Array.fromList model.posts

                maybePost =
                    Array.get idx postArray
            in
            case maybePost of
                Just post ->
                    let
                        textContent =
                            post.textContent

                        newCollapsed =
                            not textContent.collapsed

                        newTextContent =
                            { textContent | collapsed = newCollapsed }

                        newPost =
                            { post | textContent = newTextContent }

                        newPostArray =
                            Array.set idx newPost postArray

                        newPosts =
                            Array.toList newPostArray
                    in
                    { model | posts = newPosts }

                Nothing ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ header []
            [ h1 [] [ text "Welcome to Teacher Mello's Class!" ]
            , nav []
                [ viewNavButton "Home"
                , viewNavButton "Calendar"
                , viewNavButton "FAQ"
                , viewNavButton "Contact Me"
                ]
            ]
        , main_ []
            [ div [] (List.indexedMap viewBlogPost model.posts)
            , button [ type_ "button", onClick (AddPost samplePost) ] [ text "+" ]
            ]
        , footer [] [ text "Copyright No One In Particular Inc." ]
        ]


viewBlogPost : Int -> BlogPost -> Html Msg
viewBlogPost idx post =
    div [ class "blogPost" ]
        [ div [ class "blogDate" ]
            [ p [] [ text post.date ] ]
        , div [ class "blogText" ]
            [ div [ class "blogHeader" ]
                [ h2 [] [ text post.title ]
                , button [ type_ "button", class "expandToggle", onClick (ToggleCollapse idx) ]
                    [ text (viewCollapseLabel post.textContent.collapsed) ]
                ]
            , viewBlogTextContent idx post.textContent
            ]
        ]


viewCollapseLabel : Bool -> String
viewCollapseLabel collapsed =
    if collapsed then
        "v"

    else
        "^"


viewNavButton : String -> Html msg
viewNavButton t =
    a [ class "navButton", href "https://google.com" ] [ text t ]


viewBlogTextContent : idx -> BlogTextContent -> Html msg
viewBlogTextContent idx content =
    if content.collapsed then
        div [] []

    else
        div [ class "textContent" ]
            [ p [] [ text content.text ]
            , img [ alt content.image.alt, src content.image.src ] []
            ]


samplePost : BlogPost
samplePost =
    BlogPost "Oct 8" "Gad Zooks!" (BlogTextContent sampleText (BlogImage "Red Panda" sampleImgSrc) False)


sampleImgSrc : String
sampleImgSrc =
    "https://upload.wikimedia.org/wikipedia/commons/6/6d/Red_Panda_%2825193861686%29.jpg"


sampleText : String
sampleText =
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
    eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
    enim ad minim veniam, quis nostrud exercitation ullamco laboris
    nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
    reprehenderit in voluptate velit esse cillum dolore eu fugiat
    nulla pariatur. Excepteur sint occaecat cupidatat non proident,
    sunt in culpa qui officia deserunt mollit anim id est laborum.
    """
