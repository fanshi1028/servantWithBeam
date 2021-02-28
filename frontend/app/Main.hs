import Reflex.Dom
import Prelude
-- import Universum

main = mainWidget $ el "div" $ do
 el "p" $ text "Reflex is:"
 el "ul" $ do
   el "li" $ text "Efficient"
   el "li" $ text "Higher-order"
   el "li" $ text "Glitch-free"
