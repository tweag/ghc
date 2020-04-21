module TcMatches where
import GHC.Hs           ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence       ( HsWrapper )
import GHC.Types.Name   ( Name )
import TcType           ( ExpSigmaType, TcRhoType )
import TcRnTypes        ( TcM )
import GHC.Types.SrcLoc ( Located )
import GHC.Hs.Extension ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpSigmaType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
