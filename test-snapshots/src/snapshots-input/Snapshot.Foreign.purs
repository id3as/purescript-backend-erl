module Snapshot.Foreign where

import Prelude

import Effect (Effect)
import Effect.Exception (throwException)
import Effect.Exception as Exception
import Erl.Atom (Atom, atom)
import Erl.Atom as Atom
import Erl.Data.Binary (Binary)
import Erl.Data.Binary as Binary
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.Binary.IOList (IOList)
import Erl.Data.Binary.IOList as IOList
import Erl.Data.Bitstring as Bitstring
import Erl.Data.Tuple (Tuple2, tuple2, uncurry2)
import Erl.Kernel.Exceptions (error, exit, throw)
import Foreign (unsafeToForeign)

atomId :: Tuple2 String Atom -> Tuple2 String Atom
atomId = uncurry2 \s a ->
  tuple2 (Atom.toString (atom s)) (atom (Atom.toString a))

atomEq :: { true :: Boolean, false :: Boolean }
atomEq =
  { true: atom "abcd" == atom "abcd"
  , false: atom "abcd" == atom "1234"
  }

coercions =
  { binary2bitstring: \b -> Bitstring.fromBinary b
  , iolist2iodata: \l -> IOData.fromIOList l
  }

memptys =
  { string: mempty :: String
  , binary: mempty :: Binary
  , iolist: mempty :: IOList
  , iodata: mempty :: IOData
  }

appends =
  { string: \a b -> append a b :: String
  , binary: \a b -> append a b :: Binary
  , iolist: \a b -> append a b :: IOList
  , iodata: \a b -> append a b :: IOData
  }

byteSizes =
  { binary: \b -> Binary.byteSize b
  , bitstring: \b -> Bitstring.byteSize b
  , iodata: \d -> IOData.byteSize d
  , iolist: \l -> IOList.byteSize l
  }

exceptions ::
  { throw :: Effect Unit
  , error :: Effect Unit
  , exit :: Effect Unit
  , throwException :: Effect Unit
  }
exceptions =
  { throw: throw (unsafeToForeign "throw")
  , error: error (unsafeToForeign "error")
  , exit: exit (unsafeToForeign "exit")
  , throwException: throwException (Exception.error "throwException")
  }
