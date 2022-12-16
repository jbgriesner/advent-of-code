module Common.Day
  ( Day
      ( Day,
        run,
        isDefault,
        name,
        friendlyName
      ),
  )
where

data Day = Day
  { run :: IO (),
    isDefault :: Bool,
    name :: String,
    friendlyName :: String
  }