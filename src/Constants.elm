module Constants exposing (..)


courseIds : List String
courseIds =
    [ "intro-to-machine-learning"
    , "intro-to-deep-learning"
    , "intro-to-logistic-regression"
    , "study-path-and-where-to-find-resources"
    , "pytorch-tensor-manipulation"
    , "2d-point-classification-logistic-regression"
    , "mnist-shallow-deep-and-cnn"
    ]


markdownCourseIds : List String
markdownCourseIds =
    [ "intro-to-machine-learning"
    , "intro-to-deep-learning"
    , "intro-to-logistic-regression"
    ]


courseNames : List String
courseNames =
    [ "Intro To Machine Learning"
    , "Intro to Deep Learning"
    , "Intro To Logistic Regression"
    , "Study Path And Where To Find Resources"
    , "Pytorch: Tensor Manipulation"
    , "2D Point Classification: Logistic Regression"
    , "MNIST: shallow, deep, and CNN"
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
    "https://www.aiwaffle.com/"


forumRoot : String
forumRoot =
    "https://aiwaffle.flarum.cloud/embed/"


discussionIds : List String
discussionIds =
    [ "3-intro-to-machine-learning"
    , "5-intro-to-deep-learning"
    , "6-intro-to-logistic-regression"
    , "7-study-path-and-where-to-find-resources"
    , "8-pytorch-tensor-manipulation"
    , "9-2d-point-classification-logistic-regression"
    , "10-mnist-shallow-deep-and-cnn"
    ]
