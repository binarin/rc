import Safe (headDef)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Battery
import System.Taffybar.SimpleClock
import System.Taffybar.Widgets.PollingGraph
import System.Information.CPU

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
      pager = taffyPagerNew defaultPagerConfig
      tray = systrayNew
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      battery = batteryBarNew defaultBatteryConfig 30
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , endWidgets = [ tray, clock, cpu, battery ]
                                        , monitorNumber = chosenMonitorNumber
                                        }
