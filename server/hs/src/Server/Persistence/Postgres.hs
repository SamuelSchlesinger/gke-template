{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server.Persistence.Postgres where

import Data.ByteString (ByteString)
import Server.Prelude
import Squeal.PostgreSQL

type EmptySchemata :: SchemasType
type EmptySchemata =
  '[ "public" ::: '[]
   ]

type CurrentSchemata :: SchemasType
type CurrentSchemata =
  '[ "public" ::: '[ UserTable ]
   ]
    
type UserConstraints =
  '[ "user_primary_key" ::: 'PrimaryKey '["id"]
   ]

type UserColumns =
  '[ "id" ::: 'NoDef :=> 'NotNull 'PGtext
   ]

type UserTable = "user" ::: 'Table (UserConstraints :=> UserColumns)

defaultConnectionString :: ByteString
defaultConnectionString =
  "host=postgres.default.svc.cluster.local port=5432 user=postgres password=password dbname=postgres"

migrateWith :: ByteString -> IO ()
migrateWith = flip mainMigrateIso migrations

migrate :: IO ()
migrate = migrateWith defaultConnectionString

migrations :: Path (Migration (IsoQ Definition)) EmptySchemata CurrentSchemata
migrations = Migration "user" (IsoQ upUser downUser) :>> id

upUser :: Definition EmptySchemata CurrentSchemata
upUser = createTable (#public ! #user) (notNullable text `as` #id) (primaryKey #id `as` #user_primary_key)

downUser :: Definition CurrentSchemata EmptySchemata
downUser = dropTable (#public ! #user)
