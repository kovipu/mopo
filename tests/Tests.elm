module Tests exposing (..)

import Expect
import Test exposing (..)
import WeechatMessage exposing (Message(..))


all : Test
all =
    describe "A Test Suite"
        [ test "info version" <|
            \_ ->
                let
                    message =
                        [ 0, 0, 0, 30, 0, 255, 255, 255, 255, 105, 110, 102, 0, 0, 0, 7, 118, 101, 114, 115, 105, 111, 110, 0, 0, 0, 3, 51, 46, 49 ]

                    expected =
                        Info ( "version", "3.1" )
                in
                Expect.equal expected (WeechatMessage.parse message)
        , test "hdata buffer:gui_buffers(*) number,full_name" <|
            \_ ->
                let
                    message =
                        [ 0, 0, 0, 124, 0, 255, 255, 255, 255, 104, 100, 97, 0, 0, 0, 6, 98, 117, 102, 102, 101, 114, 0, 0, 0, 24, 110, 117, 109, 98, 101, 114, 58, 105, 110, 116, 44, 102, 117, 108, 108, 95, 110, 97, 109, 101, 58, 115, 116, 114, 0, 0, 0, 2, 12, 55, 102, 98, 53, 49, 56, 100, 48, 97, 102, 51, 48, 0, 0, 0, 1, 0, 0, 0, 12, 99, 111, 114, 101, 46, 119, 101, 101, 99, 104, 97, 116, 12, 55, 102, 98, 53, 49, 56, 99, 51, 53, 50, 98, 48, 0, 0, 0, 2, 0, 0, 0, 16, 114, 101, 108, 97, 121, 46, 114, 101, 108, 97, 121, 46, 108, 105, 115, 116 ]

                    expected =
                        Hdata
                            [ { fullName = "core.weechat"
                              , hpath = "7fb518d0af30"
                              , number = 1
                              }
                            , { fullName = "relay.relay.list"
                              , hpath = "7fb518c352b0"
                              , number = 2
                              }
                            ]
                in
                Expect.equal expected (WeechatMessage.parse message)
        ]
