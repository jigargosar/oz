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

    MainDoc
    {   dict: Dict Id Node
        selectedNode: Id
        selections: List Id
    }

    ZoomIn
    {   dict: Dict Id Node
        selectedNode: Id
        selections: List Id
    }



    with node's pointing to their maybe parentId.
    root nodes will have Nothing as their parentId



    Impossible State:
    * Node having multiple parents.

    Model allows for following impossible states.
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

    Tree/Forest/Zipper

    - How do we model multi-selection?

-}
