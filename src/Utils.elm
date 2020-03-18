module Utils exposing (..)


eqBy func a b =
    func a == func b


neqBy func a b =
    func a /= func b
