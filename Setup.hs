{-:set -XOverloadedStrings-}
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Client
import System.IO
import Data.List.Split
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.Directory

{-Главная функция-}
sayMeWeather = do
            grad <- weather
            putStrLn (show(fst grad) ++ " C, " ++ (snd grad))
            removeFile "cityWeather.json"
            removeFile "cityWeather2.json"
            removeFile "cityCode.json"
            removeFile "cityCodeTemp.json"

-------------------------------------------
{-ЗАПРОС ПОГОДЫ ГОРОДА -}
weather = do
         requestWeather
         fromHandleThird <- openFile "cityWeather.json" ReadMode
         contents <- hGetContents fromHandleThird
         let rawJson1 = tail contents
         let rawJson2 = take ((length rawJson1)-1) rawJson1
         writeFile "cityWeather2.json" rawJson2
         rawJson <- B.readFile "cityWeather2.json"
         let result = decodeStrict rawJson :: Maybe Object
         let clouds = decodeStrict rawJson :: Maybe Object
         return ((case result of
             Nothing -> -100
             Just info -> getWeather info),
                  (case clouds of
             Nothing -> "Invalid Json!"
             Just info -> getCloud info))

getWeather :: Object -> Double
getWeather infoWeather = 
    case parseMaybe extractWeatherInfo infoWeather of
                  Nothing -> -100
                  Just key -> key
              where 
                  extractWeatherInfo = \info -> info .: "Temperature" >>= (.: "Metric") >>= (.: "Value")


getCloud :: Object -> String
getCloud infoWeather = 
    case parseMaybe extractWeatherInfo infoWeather of
                  Nothing -> ""
                  Just key -> key
              where 
                  extractWeatherInfo = \info -> info .: "WeatherText"


--Получение ответа на запрос в файл
requestWeather = do
                 contents <- createLink
                 q <- simpleHttp contents
                 L.writeFile "cityWeather.json" q

--Формирование ссылки
createLink = do
             contents <- code
             return $ "http://apidev.accuweather.com/currentconditions/v1/" ++ (read $ show contents :: String) ++".json?language=en&apikey=hoArfRosT1215"

--------------------------------------------
{-ПОЛУЧЕНИЕ КОДА ЗАПРАШИВАЕМОГО ГОРОДА-}
code = do
    requestCode
    fromHandleThird <- openFile "cityCode.json" ReadMode
    contents <- hGetContents fromHandleThird
    let rawJson1 = tail contents
    let rawJson2 = take ((length rawJson1)-1) rawJson1
    let raw = splitOn "},{" rawJson2
    let rawJSON = head raw ++ "}"
    if length raw == 1 then writeFile "cityCodeTemp.json" rawJson2 else writeFile "cityCodeTemp.json" rawJSON
    rawJson <- B.readFile "cityCodeTemp.json"
    let result = decodeStrict rawJson :: Maybe Object
    return $ case result of
        Nothing -> "Invalid Json!"
        Just info -> getKey info


getKey :: Object -> String
getKey info = 
    case parseMaybe extractKeyInfo info of
                  Nothing -> ""
                  Just key -> key
              where 
                  extractKeyInfo = \info -> info.: "Key"

---------------------------------------------
{-ФОРМИРОВАНИЕ URL для ЗАПРОСА-}
makeUrl = do 
    putStrLn "Enter your city: "
    a <- getLine
    return $ "http://apidev.accuweather.com/locations/v1/search?q=" ++ (read $ show a :: String) ++ "&apikey=hoArfRosT1215"

requestCode = do
    contents <- makeUrl
    q <- simpleHttp contents
    L.writeFile "cityCode.json" q
