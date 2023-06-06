module Seminal.Enumerator.Signatures (enumerateChangeInSignature) where
import GHC (Sig, GhcPs)
import Seminal.Enumerator.Enumerator (Enumerator)

enumerateChangeInSignature :: Enumerator (Sig GhcPs)
enumerateChangeInSignature _ _ = []