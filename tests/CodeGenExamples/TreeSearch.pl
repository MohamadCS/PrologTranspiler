

Max(X,Y) :: ( if X >= Y then X else Y ).

MaxTree(Root) :: (

   Root <- node(V, LeftChild,RightChild);

   if LeftChild = nil, RightChild = nil then (
        V
   ) else if RightChild = nil then (
        Max(V,MaxTree(LeftChild))
   ) else if LeftChild = nil then (
        Max(V,MaxTree(RightChild))
   ) else (
        MaxChild <- Max(MaxTree(RightChild),MaxTree(LeftChild));
        Max(MaxChild,V)
   )
)
.


