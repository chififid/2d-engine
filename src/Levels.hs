module Levels where

import Base
import Vec

linkWorld :: World
linkWorld =
    createChain
        ( World
            ( addSimObjs
                [ createCircle
                    1
                    (Vec (-20) 0)
                    (Vec 0 0)
                    0
                , createCircle
                    1
                    (Vec (20) 0)
                    (Vec 0 0)
                    0
                , createAABB
                    (Vec (-5) (-100))
                    (Vec 5 100)
                    (Vec 20 0)
                    (Vec 0 0)
                , createAABB
                    (Vec (-5) (-100))
                    (Vec 5 100)
                    (Vec (-20) 0)
                    (Vec 0 0)
                ]
                []
            )
            [createField
                (Vec (-100) 0)
                (Vec 100 200)
                (Vec 0 100)
                Air
            ]
            [ SimLink 5 1 2
            , SimLink 23 2 2
            ]
            0
            ""
        )
        (createCircle
            1
            (Vec (-18) 0)
            (Vec 0 0)
            1)
        (Vec 2 0)
        19

emptyWorld = World
    ( addSimObjs
        []
        []
    )
    []
    []
    0
    ""

testWorld :: World
testWorld =
    World
        ( addSimObjs
            [ createAABB
                (Vec (-400) (-5))
                (Vec 400 5)
                (Vec 0 (-400))
                (Vec 0 0)
            , createCircle
                50
                (Vec 0 70)
                (Vec 0 0)
                0
            , createAABB
                (Vec (-5) (-400))
                (Vec 5 400)
                (Vec 400 0)
                (Vec 0 0)
            , createAABB
                (Vec (-5) (-400))
                (Vec 5 400)
                (Vec (-400) 0)
                (Vec 0 0)
            ]
            []
        )
        [ createField
            (Vec (-400) (-400))
            (Vec 400 400)
            (Vec 0 0)
            Water
        ]
        []
        0
        ""

waterWorld :: World
waterWorld =
    World
        ( addSimObjs
            [ createAABB
                (Vec (-400) (-5))
                (Vec 400 5)
                (Vec 0 (-5))
                (Vec 0 0)
            , createCircle
                0.05
                (Vec (-100) 2)
                (Vec 500 0)
                8
            ]
            []
        )
        [ createField
            (Vec (-10) (-2))
            (Vec 10 2)
            (Vec 0 2)
            Water
        ]
        []
        0
        ""

fallWorld :: World
fallWorld =
    World
        ( addSimObjs
            [ createAABB
                (Vec (-400) (-5))
                (Vec 400 5)
                (Vec 0 0)
                (Vec 0 0)
            , createCircle
                1.5
                (Vec (-50) 1500)
                (Vec 0 0)
                1
            , createCircle
                1.5
                (Vec 50 1500)
                (Vec 0 0)
                1
            ]
            []
        )
        [ createField
            (Vec (-50) (-1000))
            (Vec 50 1000)
            (Vec (-50) 1000)
            Air
        ]
        []
        0
        ""

fieldsWorld :: World
fieldsWorld =
    World
        ( addSimObjs
            [ createAABB
                (Vec (-400) (-5))
                (Vec 400 5)
                (Vec 0 0)
                (Vec 0 0)
            , createCircle
                7
                (Vec 0 30)
                (Vec 0 0)
                0.5
            , createCircle
                2
                (Vec 20 40)
                (Vec 0 0)
                0.5
            , createCircle
                5
                (Vec (-140) 30)
                (Vec 0 0)
                0.5
            , createCircle
                5
                (Vec 140 30)
                (Vec 0 0)
                0.5
            , createAABB
                (Vec (-5) (-25))
                (Vec 5 25)
                (Vec 200 25)
                (Vec 0 0)
            ]
            -- []
            []
        )
        [ createField
            (Vec (-50) (-25))
            (Vec 50 25)
            (Vec 0 30)
            Water
        , createField
            (Vec (-50) (-25))
            (Vec 50 25)
            (Vec 140 30)
            Wind
        , createField
            (Vec (-50) (-25))
            (Vec 50 25)
            (Vec (-140) 30)
            Air
        ]
        []
        0
        ""
