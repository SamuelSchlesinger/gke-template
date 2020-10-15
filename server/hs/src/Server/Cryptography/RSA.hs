module Server.Cryptography.RSA where

data RSAConfig = RSAConfig
  { publicExponent :: Integer
  , rsaKeySize :: Int
  }
