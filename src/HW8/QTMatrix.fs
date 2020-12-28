module QTMatrix

type QTree<'t> =
    | Nil
    | Leaf of 't
    | Node of QTree<'t> * QTree<'t> * QTree<'t> * QTree<'t>
