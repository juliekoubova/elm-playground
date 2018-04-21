module Main exposing (..)

import Html exposing (Html, beginnerProgram, button, li, text, ul)
import Html.Events exposing (onClick)


main : Program Never TreeNode Msg
main =
    beginnerProgram { model = model, view = view, update = update }


type alias TreeNode =
    { title : String
    , children : TreeNodeChildren
    , expanded : Bool
    }


type TreeNodeChildren
    = TreeNodeChildren (List TreeNode)


treeNode : Bool -> String -> List TreeNode -> TreeNode
treeNode expanded title children =
    { title = title
    , children = TreeNodeChildren children
    , expanded = expanded
    }


treeNodeCollapsed : String -> List TreeNode -> TreeNode
treeNodeCollapsed =
    treeNode False


model : TreeNode
model =
    treeNode True
        "Root"
        [ treeNodeCollapsed "Child 1" []
        , treeNodeCollapsed "Child 2"
            [ treeNodeCollapsed "SubChild 2A" []
            , treeNodeCollapsed "SubChild 2B" []
            ]
        ]


type Msg
    = Toggle (List String)


toggleNode : TreeNode -> List String -> TreeNode
toggleNode ({ expanded } as node) remainingPath =
    let
        (TreeNodeChildren children) =
            node.children
    in
    { node
        | expanded =
            case remainingPath of
                [] ->
                    not expanded

                _ ->
                    expanded
        , children =
            TreeNodeChildren
                (List.map (update (Toggle remainingPath)) children)
    }


update : Msg -> TreeNode -> TreeNode
update (Toggle path) node =
    case path of
        title :: remainingPath ->
            if title == node.title then
                toggleNode node remainingPath
            else
                node

        [] ->
            node


viewToggleButton : List String -> Bool -> Html Msg
viewToggleButton thisPath expanded =
    button [ onClick (Toggle (List.reverse thisPath)) ]
        [ text
            (if expanded then
                "-"
             else
                "+"
            )
        ]


viewChildren : List String -> Bool -> TreeNodeChildren -> List (Html Msg)
viewChildren parentPath expanded (TreeNodeChildren children) =
    if expanded then
        [ ul [] (List.map (viewNodeWithParent parentPath) children) ]
    else
        []


viewNodeWithParent : List String -> TreeNode -> Html Msg
viewNodeWithParent parentPath ({ children, expanded, title } as node) =
    let
        thisPath =
            title :: parentPath
    in
    li []
        (viewToggleButton thisPath expanded
            :: text title
            :: viewChildren thisPath expanded children
        )


view : TreeNode -> Html Msg
view =
    viewNodeWithParent []
