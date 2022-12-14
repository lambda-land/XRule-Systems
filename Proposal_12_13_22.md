# Type Class Structure Change Proposal
*December 13, 2022*

Currently, we have the data type and class definitions
```haskell
type Env val = [(Name, val)]

data Judge exp val = J (Env val) exp val

class Explain expr val where
  premises :: Judge expr val -> [Judge expr val]

data Proof exp val = Node (Judge exp val) [Proof exp val]

```
This restricts us to judgments that have an evaluation/functional sense, with a context. We cannot express judgments such as `add(S S 0, S 0, S S S 0)` with these definitions. We should use the more general definitions
```haskell
class Explain judge where
  premises :: judge -> [judge]

data Proof judge = Node judge [Proof judge]
```
Any judgment type that instantiates `Explain`, has the property that any judgment can be deterministically assigned to the premises that entail it. **Note** that rule systems with many derivations for the same conclusion pattern, will pretty much never be representable (such as logics or equational systems). 




Also, by parameterizing over the type of judgments, we have
```haskell
proof :: Explain judge => judge -> Proof judge
proof j = Node j (map proof (premises j))

hide :: Eq judge => judge -> Proof judge -> Maybe (Proof judge)
hide j (Node j' ps) | j == j'   = Nothing
                    | otherwise = ...
```