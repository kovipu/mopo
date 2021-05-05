module Tests exposing (..)

import Expect
import Test exposing (..)
import WeechatMessage


all : Test
all =
    describe "A Test Suite"
        [ test "info version" <|
            \_ ->
                let
                    message =
                        [ 0, 0, 0, 30, 0, 255, 255, 255, 255, 105, 110, 102, 0, 0, 0, 7, 118, 101, 114, 115, 105, 111, 110, 0, 0, 0, 3, 51, 46, 49 ]

                    expected =
                        { operation = "inf"
                        , data = ( "version", "3.1" )
                        }
                in
                Expect.equal expected (WeechatMessage.parse message)
        ]
