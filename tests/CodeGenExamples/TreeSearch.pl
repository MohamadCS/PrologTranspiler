

Max(X,Y) :: (if X >= Y then (X) else (Y)).

MaxTree(Root) ::(
   Root <- node(V, LeftChild,RightChild);
   if LeftChild = nil, RightChild = nil then (
        Result <- V;
   ) else if RightChild = nil then (
        Result <- Max(V,MaxTree(RightChild));
   ) else if LeftChild = nil then (
        Result <- Max(V,MaxTree(LeftChild));
   ) else (
        T <- Max(MaxTree(RightChild),MaxTree(LeftChild));
        Result <- Max(T,V);
   );

   Result
)
.


