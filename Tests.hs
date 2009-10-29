module Main
where

import Test.HUnit
import Cities

main = runTestTT $ TestList tests

tests = unitTests ++ acceptanceTests

unitTests = [
    -- "cityPairs - for a single user, single pair"
    --   ~: cityPairs ("matt", ["london", "paris"])
    --   ~?= [("london", "paris")],
    --
    -- "cityPairs - for a single user, multiple pairs"
    --   ~: cityPairs ("matt", ["london", "paris", "nyc"])
    --   ~?= [("london", "paris"),("london","nyc"),("paris","nyc")],
    --
	"parseRawInput"
    ~: parseRawInput "matt,london\nmatt,paris\ndave,london"
    ~?= [("matt", "london"),("matt", "paris"),("dave","london")],

    "parseRawPairs - for multiple users"
      ~: parseRawPairs [("matt", "paris"),("dave","london"),("matt", "london")]
      ~?= [("matt", ["london","paris"]),("dave",["london"])]
  ]

acceptanceTests = [
    "single user, no affinities"
      ~: affinities [("matt", ["london"])]
      ~?= [],

    "single user, single affinity"
      ~: affinities [("matt", ["london", "paris"])]
      ~?= [(("london", "paris"), 1)],

    "two users, three afinities"
      ~: affinities [("matt", ["london","paris","nyc"]),("sue", ["london", "nyc"]),("dave", ["london","nyc"])]
      ~?= [(("london","nyc"),3),(("london","paris"),1),(("paris","nyc"),1)]
  ]

