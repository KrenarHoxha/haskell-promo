import Data.Char (toLower)
import System.IO (readFile')

rng :: Int -> IO Int
rng _ = return 0


gameLoop :: IO ()
gameLoop = do
  putStrLn "Du befindest dich in einem Bueschel hohem Gras."
  putStrLn "Was willst du tun? (Gehen/beenden)"

  input <- getLine

  if map toLower input == "beenden"
    then putStrLn "Game Over!"
    else do
      putStrLn "Du gehst durch das hohe Gras..."

      randomNumber <- rng 1

      if randomNumber == 0
        then randomEncounter
        else putStrLn "Nichts passiert"

      gameLoop


randomEncounter :: IO ()
randomEncounter = do
  pokemon <- getRandomPokemon

  putStrLn ("Ein wildes " ++ pokemon ++ " taucht auf!")

  caught <- isCaught pokemon

  if caught
    then putStrLn ("Du hast " ++ pokemon ++ " schon gefangen!")
    else catchPkmn pokemon


getRandomPokemon :: IO String
getRandomPokemon = do
  content <- readFile "pokemon.txt"

  let pokemonList = lines content

  number <- rng (length pokemonList - 1)

  return (pokemonList !! number)


isCaught :: String -> IO Bool
isCaught pokemon = do
  content <- readFile' "caught.txt"
  return (pokemon `elem` lines content)


catchPkmn :: String -> IO ()
catchPkmn pokemon = do
  putStrLn "Willst du das Pokemon fangen? (Ja/Nein)"
  answer <- getLine

  if map toLower answer == "nein"
    then putStrLn "Du bist entkommen!"
    else do
      appendFile "caught.txt" (pokemon ++ "\n")
      putStrLn (pokemon ++ " wurde gefangen!")