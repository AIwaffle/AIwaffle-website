module Style exposing (color, font)

import Element as E
import Element.Font as Font


font =
    { title =
        [ Font.size 26
        , Font.bold
        , E.paddingXY 0 15
        ]
    , subtitle =
        [ Font.size 22
        , Font.bold
        , E.paddingXY 0 10
        ]
    , link =
        [ Font.underline
        ]
    }


color =
    { yellow = E.rgb255 247 203 55
    , darkYellow = E.rgb255 235 182 0
    , grey = E.rgb255 170 170 170
    , dark = E.rgb255 50 29 29
    , black = E.rgb255 0 0 0
    }
