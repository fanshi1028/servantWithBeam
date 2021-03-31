import Universum
import Utils.Head (myMainWidget)
import Widgets.Currency (currencyFetcher)

main :: IO ()
main = myMainWidget currencyFetcher
