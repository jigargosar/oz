module Doc exposing (..)

{-|

    FEATURES

  - not empty, at least one node, even if text is empty
  - expand/collapse: collapsed node not considered when navigating and rendering
      - But we do need to differentiate nodes having collapsed children.
  - zoom in on a node
  - search: i.e. filter nodes matching certain criteria. including collapsed nodes.
      - We will need to distinguish collapsed nodes which are temporarily expanded.
  - single selection mode by default.
  - and multi-selection mode for bulk edit.

-}


{-|

    Possible Models

    type alias MainDoc =
        {   dict: Dict Id Node
            selectedNode: Id
            selections: List Id
        }

    ZoomIn
        {   dict: Dict Id Node
            selectedNode: Id
            selections: List Id
        }

    type alias Node = { id: Id,
        title: String
        }

    Impossible State:
    * Node having multiple parents.

    Representable Invalid states.
    * Orphan Nodes:
        * Nodes with missing parent. i.e. id exists, but not the node.
        * Cyclic parentId's: nodes with no path to root node.
        * Infinite Loop: node pointing to it self. Directly or Cyclic.
    * selectedId points to non-existing node/or collapsed node.
    * same for selections: they might be collapsed, i.e. not visible.

    If we can guard against these states, then we can implement all the features.
    * selectedId, can be guarded by having non-empty Dict
    * cyclic nodes can be guarded, by ensuring every parentId modification has path to root.
        * And we can break the cycle by, replacing re-occurrence of ancestorId with Nothing for display purposes.
        * And explicitly, list orphans and cycles. during development.

    * selections can be assumed to only be valid if nodes are visible.

    * node visibility will have to be computed. based on query & collapse state.

    Tree/Forest/Zipper

    Representable Invalid states.
    * Id duplicated across different parts of tree

    Impl
    * Zoom can be managed by having a doc, and zoomDoc, where operations are performed on zoomDoc.
        * And in case of home, parent doc can be Nothing, representing that we cannot zoom out further.

    * visibility: nodeId's matching search criteria will be visible.
        * doc and pass extra info on current collapse state (no children, expanded , collapsed, tempCollapsed)
        * tempCollapsed state will have to be maintained separately, to be reset on query change,
        and on explicit user interaction. and perhaps we will have to update the model too.

    * focused node will always be visible. irrespective of search and collapse state.
    * isFocused will have to be passed by doc, instead of querying it. during restructure;
    * same with every other item field.

    * with some restructuring of code, it should be possible to handle all cases except multi-selection

    - model multi-selection?
    * perhaps, we can use different model, or patch it with either set of Id's or different zipper impl,
    with multi selections.
    * Adding above features will be complex enough, we can tackle multi-selection later.

    Ok

    With ForestZipper
    how do we implement, collapsible state.
    * strict route might be complex.
    Let's try

-}
type Tree a b
    = Tree a (Children a b)
    | Leaf a


type Children a b
    = Children b (Tree a b) (List (Tree a b))
