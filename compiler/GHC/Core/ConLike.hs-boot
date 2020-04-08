module GHC.Core.ConLike where
import {-# SOURCE #-} GHC.Core.DataCon (DataCon)
import {-# SOURCE #-} GHC.Core.PatSyn (PatSyn)
import Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name
