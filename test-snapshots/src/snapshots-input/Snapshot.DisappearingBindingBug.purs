module Snapshot.DisappearingBindingBug where

import Erl.Data.List (List, nil, (:))
import Prelude


type Opts = { secondBit :: Boolean, prefix :: Boolean }


thing ::Opts -> List String
thing opts =
  let
    prefix = "prefix" : nil
    header =
      ( if opts.prefix then
          prefix
        else
          prefix
      )
        <>
          ( if opts.secondBit then
              "secondbit" : nil
            else
              "nosecondbit" : nil
          )
  in
    header
      <>
        ("this" : "goes" : "missing" : nil)

