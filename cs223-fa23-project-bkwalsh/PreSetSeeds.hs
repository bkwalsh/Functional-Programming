module PreSetSeeds where

import Data.HashMap.Strict qualified as HashMap

blinker :: String
blinker = "x10y10 $$$b3o!"

glider :: String
glider = "x25y25 $$$2bob$3bo$b3o!"

pulser :: String
pulser =
  "x30y30 $$$$$$$2b2b3o3b3o2b2$$2bo4bobo4bo$2bo4bobo4bo$2bo4bobo4bo$2b2b3o3b3o2b2$$2b2b3o3b3o2b$2bo4bob"
    ++ "o4bo$2bo4bobo4bo$2bo4bobo4bo2$$2b2b3o3b3o!"

gliderGun :: String
gliderGun =
  "x50y50 $$$$$$$26bo11b$24bobo11b$14b2o6b2o12b2o$13bo3bo4b2o12b2o$2b2o8bo5bo3b2o14b$2b2o8b"
    ++ "o3bob2o4bobo11b$12bo5bo7bo11b$13bo3bo20b$14b2o!"

hammerHead :: String
hammerHead =
  "x50y150 $$$$$$$30b5o13b$30bo4bo7b2o3b$30bo11b2ob3o$31bo9b2ob4o$30b3b2o3b2ob2o2b2ob"
    ++ "$30b5bo4bo2bo4b$30b6bobobobo5b$30b7bo10b$30b7bo10b$30b6bobobobo5b$30b5bo4bo2bo4b$30b3b2o3b2ob2o"
    ++ "2b2ob$31bo9b2ob4o$30bo11b2ob3o$30bo4bo7b2o3b$30b5o30b!"
