module Constants exposing (..)


courseNames : List String
courseNames =
    [ "Intro to Machine Learning"
    , "Intro to Deep Learning"
    , "Logistic Regression Model"
    , "Study Path and Resources"
    , "Pytorch Tensor Manipulation"
    , "2D Point Classification"
    , "Handwritten Digit Classification"
    ]


courseDemos : List Bool
courseDemos =
    [ False
    , False
    , True
    , False
    , False
    , False
    , False
    ]


serverRoot : String
serverRoot =
    "http://106.15.39.117:8080/"