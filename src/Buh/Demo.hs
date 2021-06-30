module Buh.Demo where

data DemoReqBody =
    DemoReqBodyPing
  | DemoReqBodyBoom
  deriving stock (Eq, Show)

data DemoResBody =
    DemoResBodyPong
  deriving stock (Eq, Show)
