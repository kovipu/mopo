module Tests exposing (..)

import Expect
import Test exposing (..)
import WeechatMessage exposing (Message)


all : Test
all =
    describe "A Test Suite"
        [ test "info version" <|
            \_ ->
                let
                    message =
                        [ 0, 0, 0, 30, 0, 255, 255, 255, 255, 105, 110, 102, 0, 0, 0, 7, 118, 101, 114, 115, 105, 111, 110, 0, 0, 0, 3, 51, 46, 49 ]

                    expected =
                        Info ( Just "version", Just "3.1" )
                in
                Expect.equal expected (WeechatMessage.parse message)
        , test "(hdata_buffers) hdata buffer:gui_buffers(*) number,full_name" <|
            \_ ->
                let
                    message =
                        [ 0, 0, 0, 167, 0, 0, 0, 0, 13, 104, 100, 97, 116, 97, 95, 98, 117, 102, 102, 101, 114, 115, 104, 100, 97, 0, 0, 0, 6, 98, 117, 102, 102, 101, 114, 0, 0, 0, 39, 110, 117, 109, 98, 101, 114, 58, 105, 110, 116, 44, 102, 117, 108, 108, 95, 110, 97, 109, 101, 58, 115, 116, 114, 44, 115, 104, 111, 114, 116, 95, 110, 97, 109, 101, 58, 115, 116, 114, 0, 0, 0, 2, 12, 55, 102, 102, 101, 52, 98, 101, 56, 99, 51, 102, 48, 0, 0, 0, 1, 0, 0, 0, 12, 99, 111, 114, 101, 46, 119, 101, 101, 99, 104, 97, 116, 0, 0, 0, 7, 119, 101, 101, 99, 104, 97, 116, 12, 55, 102, 102, 101, 54, 98, 99, 53, 54, 55, 54, 48, 0, 0, 0, 2, 0, 0, 0, 16, 114, 101, 108, 97, 121, 46, 114, 101, 108, 97, 121, 46, 108, 105, 115, 116, 255, 255, 255, 255 ]

                    expected =
                        Buffers
                            [ { fullName = "core.weechat"
                              , shortName = Just "weechat"
                              , ppath = "7ffe4be8c3f0"
                              , number = 1
                              }
                            , { fullName = "relay.relay.list"
                              , shortName = Nothing
                              , ppath = "7ffe6bc56760"
                              , number = 2
                              }
                            ]
                in
                Expect.equal expected (WeechatMessage.parse message)
        ]
