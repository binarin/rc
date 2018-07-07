import Safe (headDef)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Taffybar
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Information.CPU

import System.Taffybar.Widget.SNITray

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

main = do
  args <- getArgs
  let chosenMonitorNumber = fromMaybe 0 $ readMaybe $ headDef "0" args
  let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                  , graphLabel = Just "cpu"
                                  }
      clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      -- pager = taffyPagerNew defaultPagerConfig
      tray = sniTrayNew
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      -- battery = batteryBarNew defaultBatteryConfig 30
      simpleConfig = defaultSimpleTaffyConfig
        { startWidgets = [ -- pager
                         ]
        , endWidgets = [ tray, clock, cpu
                       -- , battery
                       ]
        , monitorsAction = pure [chosenMonitorNumber]
        }
  simpleTaffybar simpleConfig
