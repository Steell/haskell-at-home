{-# language RecordWildCards #-}

module Main where

import           Api

import           Control.Concurrent.STM         ( atomically )
import           Control.Concurrent.STM.TVar    ( newTVarIO
                                                , readTVar
                                                , writeTVar
                                                )
import           Control.Concurrent.STM.TChan   ( newBroadcastTChanIO
                                                , writeTChan
                                                )

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

-- TODO: this is gross, don't overload alias
import qualified OpenZWave.Ozw                 as OZW

import qualified ReactiveDaemon.OpenZWave      as Z

import           Network.Wai.Handler.Warp       ( run )

import           Server

import           System.Environment             ( getArgs )

---

main :: IO ()
main = do
    [device]      <- getArgs
    manager       <- initOzw device
    emptyState    <- newTVarIO emptyZWaveState
    broadcastChan <- newBroadcastTChanIO
    let env = ServerEnv emptyState manager broadcastChan
    hookZWaveNotifications env
    run 8081 $ serverApp env
  where
    initOzw :: String -> IO OZW.Manager
    initOzw device = Z.initOzw Z.defaultOptions { Z._driverPath = device }

    hookZWaveNotifications :: ServerEnv -> IO ()
    hookZWaveNotifications ServerEnv {..} = do
        unregister <- Z.registerNotificationEvent _manager $ \notification -> do
            -- putStrLn $ "notif: " ++ show notification
            -- map <-
              atomically $ do
                state <- readTVar _state
                let state'@ZWaveState {..} = updateState notification state
                writeTVar _state state'
                writeTChan _stateBroadcastChan _homeMap
                -- return _homeMap
            -- putStrLn $ "newState: " ++ show map
        return ()

    -- TODO: this is a garbage fire
    updateState :: Z.Notification -> ZWaveState -> ZWaveState
    updateState n s@ZWaveState {..} = go n
      where
        go (Z.DriverReady hid) =
              let
                  homeId   = toInteger hid
                  home     = Home homeId Map.empty
              in
                  s { _homeMap = Map.insert homeId home _homeMap }

        go (Z.NodeAdded Z.NodeInfo {..}) =
            let
                deviceId = toInteger _nodeId
                homeId   = toInteger _nodeHome
                device@Device {..} =
                    Device {_deviceId = deviceId, _deviceValues = Map.empty}
            in
                s
                    { _homeMap =
                        Map.adjust
                            (\h@Home {..} -> h
                                { _homeDevices = Map.insert _deviceId
                                                            device
                                                            _homeDevices
                                }
                            )
                            homeId
                            _homeMap
                    }
        go (Z.NodeRemoved hid id)
            = let
                  deviceId = toInteger id
                  homeId   = toInteger hid
              in
                  s
                      { _homeMap =
                          Map.adjust
                              (\h@Home {..} -> h
                                  { _homeDevices = Map.delete deviceId
                                                              _homeDevices
                                  }
                              )
                              homeId
                              _homeMap
                      }
        go (Z.ValueAdded (Z.ZVID hid vid) Z.ValueInfo {..} vData)
            = let
                  valueId          = toInteger vid
                  homeId           = toInteger hid
                  deviceId         = toInteger _vInfoNode
                  value@Value {..} = Value
                      { _valueId    = valueId
                      , _valueState = convertZWaveValue vData
                      , _valueName = _vInfoName
                      }
              in
                  s
                      { _homeMap =
                          Map.adjust
                              (\h@Home {..} -> h
                                  { _homeDevices =
                                      Map.adjust
                                          (\d@Device {..} -> d
                                              { _deviceValues =
                                                  Map.insert _valueId
                                                             value
                                                             _deviceValues
                                              }
                                          )
                                          deviceId
                                          _homeDevices
                                  }
                              )
                              homeId
                              _homeMap
                      }
        go (Z.ValueRemoved hid nid vid)
            = let
                  homeId   = toInteger hid
                  deviceId = toInteger nid
                  valueId  = toInteger vid
              in
                  s
                      { _homeMap =
                          Map.adjust
                              (\h@Home {..} -> h
                                  { _homeDevices =
                                      Map.adjust
                                          (\d@Device {..} -> d
                                              { _deviceValues =
                                                  Map.delete valueId
                                                             _deviceValues
                                              }
                                          )
                                          deviceId
                                          _homeDevices
                                  }
                              )
                              homeId
                              _homeMap
                      }
        go (Z.ValueChanged hid nid vid vData)
            = let
                  homeId   = toInteger hid
                  deviceId = toInteger nid
                  valueId  = toInteger vid
              in

                  s
                      { _homeMap =
                          Map.adjust
                              (\h@Home {..} -> h
                                  { _homeDevices =
                                      Map.adjust
                                          (\d@Device {..} -> d
                                              { _deviceValues =
                                                  Map.adjust
                                                      (\v -> v
                                                          { _valueState =
                                                              convertZWaveValue
                                                                  vData
                                                          }
                                                      )
                                                      valueId
                                                      _deviceValues
                                              }
                                          )
                                          deviceId
                                          _homeDevices
                                  }
                              )
                              homeId
                              _homeMap
                      }
        go _ = s
