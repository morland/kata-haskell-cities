import Cities

main = do
    contents <- getContents
    let pairedlist = parseRawInput contents
    let multipairs = parseRawPairs pairedlist
    let results = affinities multipairs
    mapM_ print results