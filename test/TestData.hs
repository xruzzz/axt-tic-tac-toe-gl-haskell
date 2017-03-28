{-# LANGUAGE UnicodeSyntax #-}
module TestData
    (
        fields
    )
    where
import AXT.TicTacToe.Types as GT (CoorOnField, Field(F))

fields = fmap F [
            ["XOO", "   ", "   "],
            ["XXX", "   ", "   "],
            ["XXX", "OO ", "   "],
            ["XX ", "   ", "   "],
            ["XO ", "XOX", "   "]]
