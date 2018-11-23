{-# language RecordWildCards #-}

module Main where

import           Api

import           Control.Concurrent.STM         ( STM, atomically )
import           Control.Concurrent.STM.TVar    ( newTVarIO
                                                , readTVar
                                                , writeTVar
                                                )
import           Control.Concurrent.STM.TChan   ( TChan
                                                , newBroadcastTChanIO
                                                , writeTChan
                                                )

import qualified Data.Map.Strict               as Map

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
    stateChan     <- newBroadcastTChanIO
    eventChan     <- newBroadcastTChanIO
    let env = ServerEnv emptyState manager stateChan eventChan
    hookZWaveNotifications env
    run 8081 $ serverApp env
  where
    initOzw :: String -> IO OZW.Manager
    initOzw device = Z.initOzw Z.defaultOptions { Z._driverPath = device }

    hookZWaveNotifications :: ServerEnv -> IO ()
    hookZWaveNotifications ServerEnv {..} = do
        unregister <- Z.registerNotificationEvent _manager $ \notification ->
            atomically $ do
                state <- readTVar _state
                state'@ZWaveState {..} <- updateState _eventBroadcastChan notification state
                writeTVar _state state'
                writeTChan _stateBroadcastChan _homeMap
        return ()

    -- TODO: this is a garbage fire
    updateState :: TChan ZEvent -> Z.Notification -> ZWaveState -> STM ZWaveState
    updateState chan n s@ZWaveState {..} = go n
      where
        go (Z.DriverReady hid) =
            let homeId = toInteger hid
                home   = Home homeId Map.empty
            in  do
                writeTChan chan $ HomeAdded home
                return s { _homeMap = Map.insert homeId home _homeMap }

        go (Z.NodeAdded Z.NodeInfo {..}) =
            let
                deviceId = toInteger _nodeId
                homeId   = toInteger _nodeHome
                device@Device {..} =
                    Device { _deviceId = deviceId
                           , _deviceName = _nodeName
                           , _deviceManufacturer = _nodeManufacturer
                           , _deviceProductName = _nodeProductName
                           , _deviceProductType = _nodeProductType
                           , _deviceValues = Map.empty
                           }
            in do
                writeTChan chan $ DeviceAdded homeId device
                return s
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
              in do
                  writeTChan chan $ DeviceRemoved homeId deviceId
                  return s
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
        go (Z.ValueAdded hid Z.ValueInfo {..} vData)
            = let
                  valueId          = toInteger _vInfoId
                  homeId           = toInteger hid
                  deviceId         = toInteger _vInfoNode
                  value@Value {..} = Value
                      { _valueId    = valueId
                      , _valueState = convertZWaveValue vData
                      , _valueName  = _vInfoName
                      }
              in do
                  writeTChan chan $ ValueAdded homeId deviceId value
                  return s
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
              in do
                  writeTChan chan $ ValueRemoved homeId deviceId valueId
                  return s
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
                  state    = convertZWaveValue vData
              in do
                  writeTChan chan $ ValueChanged homeId deviceId valueId state
                  return s
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
                                                              state
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
        go _ = return s
