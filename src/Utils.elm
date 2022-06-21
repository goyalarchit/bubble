module Utils exposing (..)


okMessage : String -> String
okMessage text =
    "Control ~" ++ text ++ "~ captured."


errMessage : String -> String
errMessage text =
    "Error! " ++ text
