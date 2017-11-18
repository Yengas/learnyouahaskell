module Writer where
import Control.Monad.Writer

-- Be careful with your selection of monoid. For example Lists will use ++ for concatenating in mappend. So you can have
-- a writer that causes slow down each time you call tell, because it will add to end of the list.

