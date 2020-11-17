# Making `ti :: TypeEnv -> Exp -> TI (Either String (Subst, Type))`

`ti` is referenced a lot by each of its pattern matching arms. This would
require a lot of changes (see the multiple nested `case` expressions
in `mgu`). It would be even less readable than this given that the return
type is already a tuple!

If a list of types were to be returned, another error passing mechanism
would be required. However, this, alongside implementing `MonadPlus` could
allow for a more monadic inference system to be written.
