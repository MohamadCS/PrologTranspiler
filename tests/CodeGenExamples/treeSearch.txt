


MaxTree(Root) :: (

   Max <- (X,Y) => (if X >= Y then X else Y);

   if #1 <- node(V, LeftChild,RightChild) | LeftChild = nil, RightChild = nil then (
        write(V),
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


