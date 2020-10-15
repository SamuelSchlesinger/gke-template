module Main where

import Server.Persistence.Postgres (migrate)

main :: IO ()
main = migrate
