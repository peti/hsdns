{-# OPTIONS -fffi -fglasgow-exts #-}
{- |
   Module      :  Network.DNS.ADNS
   Copyright   :  (c) 2005-01-04 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   This module provides bindings to GNU ADNS, a domain name
   resolver library written in C. Its source code, among
   other things, is available at
   <http://www.gnu.org/software/adns/>. Process this module
   with @hsc2hs@, to generate the final Haskell source code,
   which interfaces to ADNS.

   You will most likely not need this module directly.
   "PollResolver" provides a much nicer interface from the
   Haskell world; this module contains basically just
   marshaling code. Nevertheless, here are some code
   examples, which show how to use the resolver
   synchronously:

   > resolve :: String -> RRType -> IO ()
   > resolve r rt = do
   >   st <- adnsInit [ LogPid ]
   >   rr <- adnsSynch st r rt [ Owner ]
   >   print rr

   Another approach is this:

   > resolveA :: String -> RRType -> IO ()
   > resolveA r rt = do
   >   st <- adnsInit [ LogPid ]
   >   q <- adnsSubmit st r rt [ Owner ]
   >   a <- getAns st q
   >   print a
   >
   >     where getAns st q = do
   >             rc <- adnsCheck st q
   >             case rc of
   >               Just a  -> return a
   >               Nothing -> do threadDelay (10^6)
   >                             getAns st q
-}

module Network.DNS.ADNS where

import Foreign
import Foreign.C
import Control.Exception ( finally, assert )
import Network           ( HostName )
import Network.Socket    ( HostAddress )
import Data.Endian

----- Mashal ADNS Data Types -----------------------------------------

#include <adns.h>
#include <errno.h>
#include <poll.h>

data OpaqueState
type RawState = Ptr OpaqueState
type State    = ForeignPtr OpaqueState

data OpaqueQuery
type Query = Ptr OpaqueQuery

data InitFlag
  = NoEnv         -- ^ do not look at environment
  | NoErrPrint    -- ^ never print output to stderr ('Debug' overrides)
  | NoServerWarn  -- ^ do not warn to stderr about duff nameservers etc
  | Debug         -- ^ enable all output to 'stderr' plus 'Debug' msgs
  | LogPid        -- ^ include pid in diagnostic output
  | NoAutoSys     -- ^ do not make syscalls at every opportunity
  | Eintr         -- ^ allow 'adnsSynch' to return 'eINTR'
  | NoSigPipe     -- ^ applic has SIGPIPE set to SIG_IGN, do not protect
  | CheckC_EntEx  -- ^ do consistency checks on entry\/exit to adns funcs
  | CheckC_Freq   -- ^ do consistency checks very frequently (slow!)
  deriving (Eq, Bounded, Show)

instance Enum InitFlag where
  toEnum #{const adns_if_noenv}        = NoEnv
  toEnum #{const adns_if_noerrprint}   = NoErrPrint
  toEnum #{const adns_if_noserverwarn} = NoServerWarn
  toEnum #{const adns_if_debug}        = Debug
  toEnum #{const adns_if_logpid}       = LogPid
  toEnum #{const adns_if_noautosys}    = NoAutoSys
  toEnum #{const adns_if_eintr}        = Eintr
  toEnum #{const adns_if_nosigpipe}    = NoSigPipe
  toEnum #{const adns_if_checkc_entex} = CheckC_EntEx
  toEnum #{const adns_if_checkc_freq}  = CheckC_Freq
  toEnum i = error ("ADNS.InitFlag cannot be mapped to value " ++ show i)

  fromEnum NoEnv         = #{const adns_if_noenv}
  fromEnum NoErrPrint    = #{const adns_if_noerrprint}
  fromEnum NoServerWarn  = #{const adns_if_noserverwarn}
  fromEnum Debug         = #{const adns_if_debug}
  fromEnum LogPid        = #{const adns_if_logpid}
  fromEnum NoAutoSys     = #{const adns_if_noautosys}
  fromEnum Eintr         = #{const adns_if_eintr}
  fromEnum NoSigPipe     = #{const adns_if_nosigpipe}
  fromEnum CheckC_EntEx  = #{const adns_if_checkc_entex}
  fromEnum CheckC_Freq   = #{const adns_if_checkc_freq}

data QueryFlag
  = Search            -- ^ use the searchlist
  | UseVC             -- ^ use a virtual circuit (TCP connection)
  | Owner             -- ^ fill in the owner field in the answer
  | QuoteOk_Query     -- ^ allow special chars in query domain
  | QuoteOk_CName     -- ^ allow ... in CNAME we go via (default)
  | QuoteOk_AnsHost   -- ^ allow ... in things supposed to be hostnames
  | QuoteFail_CName   -- ^ refuse if quote-req chars in CNAME we go via
  | CName_Loose       -- ^ allow refs to CNAMEs - without, get _s_cname
  | CName_Forbid      -- ^ don't follow CNAMEs, instead give _s_cname
  deriving (Eq, Bounded, Show)

instance Enum QueryFlag where
  toEnum #{const adns_qf_search}          = Search
  toEnum #{const adns_qf_usevc}           = UseVC
  toEnum #{const adns_qf_owner}           = Owner
  toEnum #{const adns_qf_quoteok_query}   = QuoteOk_Query
  toEnum #{const adns_qf_quoteok_cname}   = QuoteOk_CName
  toEnum #{const adns_qf_quoteok_anshost} = QuoteOk_AnsHost
  toEnum #{const adns_qf_quotefail_cname} = QuoteFail_CName
  toEnum #{const adns_qf_cname_loose}     = CName_Loose
  toEnum #{const adns_qf_cname_forbid}    = CName_Forbid
  toEnum i = error ("ADNS.QueryFlag cannot be mapped to value " ++ show i)

  fromEnum Search          = #{const adns_qf_search}
  fromEnum UseVC           = #{const adns_qf_usevc}
  fromEnum Owner           = #{const adns_qf_owner}
  fromEnum QuoteOk_Query   = #{const adns_qf_quoteok_query}
  fromEnum QuoteOk_CName   = #{const adns_qf_quoteok_cname}
  fromEnum QuoteOk_AnsHost = #{const adns_qf_quoteok_anshost}
  fromEnum QuoteFail_CName = #{const adns_qf_quotefail_cname}
  fromEnum CName_Loose     = #{const adns_qf_cname_loose}
  fromEnum CName_Forbid    = #{const adns_qf_cname_forbid}

-- |The record types we support.

data RRType = A | MX | NS | PTR
  deriving (Eq, Bounded, Show)

instance Enum RRType where
  toEnum #{const adns_r_a}   = A
  toEnum #{const adns_r_mx}  = MX
  toEnum #{const adns_r_ns}  = NS
  toEnum #{const adns_r_ptr} = PTR
  toEnum i = error ("ADNS.RRType cannot be mapped to value " ++ show i)

  fromEnum A   = #{const adns_r_a}
  fromEnum MX  = #{const adns_r_mx}
  fromEnum NS  = #{const adns_r_ns}
  fromEnum PTR = #{const adns_r_ptr}

instance Storable RRType where
  sizeOf _     = #{size adns_rrtype}
  alignment _  = alignment (undefined :: #{type adns_rrtype})
  poke ptr t   = let p = castPtr ptr :: Ptr #{type adns_rrtype}
                 in poke p ((toEnum . fromEnum) t)
  peek ptr     = let p = castPtr ptr :: Ptr #{type adns_rrtype}
                 in peek p >>= return . toEnum . fromEnum

-- |Because ADNS' status codes may vary with different
-- versions of the library, I don't want to provide a
-- \"comprehensive\" list by defining an enum type. Instead,
-- the 'Status' type contains the numeric value as returned
-- by ADNS itself.
--
-- For common status codes, helper functions are provided,
-- which construct the appropriate 'Status' instance.
-- Examples are 'sOK' or 'sNXDOMAIN'. The functions
-- 'adnsErrTypeAbbrev', 'adnsErrAbbrev', and 'adnsStrerror'
-- can also be used to map these codes into human readable
-- strings.

newtype Status  = StatusCode Int
  deriving (Eq, Show)

#enum Status, StatusCode \
  , sOK                   = adns_s_ok \
  , sNOMEMORY             = adns_s_nomemory \
  , sUNKNOWNRRTYPE        = adns_s_unknownrrtype \
  , sSYSTEMFAIL           = adns_s_systemfail \
  , sMAX_LOCALFAIL        = adns_s_max_localfail \
  , sTIMEOUT              = adns_s_timeout \
  , sALLSERVFAIL          = adns_s_allservfail \
  , sNORECURSE            = adns_s_norecurse \
  , sINVALIDRESPONSE      = adns_s_invalidresponse \
  , sUNKNOWNFORMAT        = adns_s_unknownformat \
  , sMAX_REMOTEFAIL       = adns_s_max_remotefail \
  , sRCODESERVFAIL        = adns_s_rcodeservfail \
  , sRCODEFORMATERROR     = adns_s_rcodeformaterror \
  , sRCODENOTIMPLEMENTED  = adns_s_rcodenotimplemented \
  , sRCODEREFUSED         = adns_s_rcoderefused \
  , sRCODEUNKNOWN         = adns_s_rcodeunknown \
  , sMAX_TEMPFAIL         = adns_s_max_tempfail \
  , sINCONSISTENT         = adns_s_inconsistent \
  , sPROHIBITEDCNAME      = adns_s_prohibitedcname \
  , sANSWERDOMAININVALID  = adns_s_answerdomaininvalid \
  , sANSWERDOMAINTOOLONG  = adns_s_answerdomaintoolong \
  , sINVALIDDATA          = adns_s_invaliddata \
  , sMAX_MISCONFIG        = adns_s_max_misconfig \
  , sQUERYDOMAINWRONG     = adns_s_querydomainwrong \
  , sQUERYDOMAININVALID   = adns_s_querydomaininvalid \
  , sQUERYDOMAINTOOLONG   = adns_s_querydomaintoolong \
  , sMAX_MISQUERY         = adns_s_max_misquery \
  , sNXDOMAIN             = adns_s_nxdomain \
  , sNODATA               = adns_s_nodata \
  , sMAX_PERMFAIL         = adns_s_max_permfail

-- |Original definition:
--
-- >    typedef struct {
-- >      int len;
-- >      union {
-- >        struct sockaddr sa;
-- >        struct sockaddr_in inet;
-- >      } addr;
-- >    } adns_rr_addr;
--
-- /Note/: Anything but @sockaddr_in@ will cause 'peek' to call 'fail',
-- when marshaling this structure.

newtype RRAddr = RRAddr HostAddress
  deriving (Eq)

instance Show RRAddr where
  show (RRAddr ha) = shows b1 . ('.':) .
                     shows b2 . ('.':) .
                     shows b3 . ('.':) .
                     shows b4 $ ""
    where
    (b1,b2,b3,b4) = ha2tpl ha

instance Storable RRAddr where
  sizeOf _    = #{size adns_rr_addr}
  alignment _ = alignment (undefined :: CInt)
  poke _ _    = error "poke is undefined for ADNS.RRAddr"
  peek ptr'   = do
    let ptr = #{ptr adns_rr_addr, addr} ptr'
    (t :: #{type sa_family_t}) <- #{peek struct sockaddr_in, sin_family} ptr
    if (t /= #{const AF_INET})
       then fail ("peek ADNS.RRAddr: unsupported 'sockaddr' type " ++ show t)
       else #{peek struct sockaddr_in, sin_addr} ptr >>= return . RRAddr

-- |Original definition:
--
-- >    typedef struct {
-- >      char *host;
-- >      adns_status astatus;
-- >      int naddrs; /* temp fail => -1, perm fail => 0, s_ok => >0
-- >      adns_rr_addr *addrs;
-- >    } adns_rr_hostaddr;
--
-- The @naddrs@ field is not available in @RRHostAddr@, because I
-- couldn't see how that information wouldn't be available in the
-- @astatus@ field, too. If I missed anything, someone let me know.
--
-- /Note/: Eventually, I should return 'HostAddress' rather than
-- 'RRAddr'. I'm using the former only, because it has nicer output
-- with 'show'.

data RRHostAddr = RRHostAddr HostName Status [RRAddr]
  deriving (Show)

instance Storable RRHostAddr where
  sizeOf _    = #{size adns_rr_hostaddr}
  alignment _ = alignment (undefined :: CString)
  poke _ _    = error "poke is undefined for ADNS.RRHostAddr"
  peek ptr    = do
    h <- #{peek adns_rr_hostaddr, host} ptr
    hstr <- assert (h /= nullPtr) (peekCString h)
    st <- #{peek adns_rr_hostaddr, astatus} ptr
    (nadr :: #{type adns_status}) <- #{peek adns_rr_hostaddr, naddrs} ptr
    aptr <- #{peek adns_rr_hostaddr, addrs} ptr
    adrs <- if (nadr > 0)
                then peekArray (fromEnum nadr) aptr
                else return []
    return (RRHostAddr hstr (StatusCode st) adrs)

-- |Original definition:
--
-- >    typedef struct {
-- >      int i;
-- >      adns_rr_hostaddr ha;
-- >    } adns_rr_inthostaddr;

data RRIntHostAddr = RRIntHostAddr Int RRHostAddr
                     deriving (Show)

instance Storable RRIntHostAddr where
    sizeOf _     = #{size adns_rr_inthostaddr}
    alignment _  = alignment (undefined :: CInt)
    poke _ _     = error "poke is undefined for ADNS.RRIntHostAddr"
    peek ptr     = do
      (i::CInt) <- #{peek adns_rr_inthostaddr, i} ptr
      a <- #{peek adns_rr_inthostaddr, ha} ptr
      return (RRIntHostAddr (fromEnum i) a)

data Answer = Answer
  { status  :: Status
      -- ^ Status code for this query.
  , cname   :: Maybe String
      -- ^ Always 'Nothing' for 'CNAME' queries (which are not supported yet anyway).
  , owner   :: Maybe String
      -- ^ Only set if 'Owner' was requested for query.
  , expires :: CTime
      -- ^ Only defined if status is 'sOK', 'sNXDOMAIN', or 'sNODATA'.
  , rrs     :: [Response]
      -- ^ The list will be empty if an error occured.
  }
  deriving (Show)

data Response
  = RRA RRAddr
  | RRMX Int RRHostAddr
  | RRNS RRHostAddr
  | RRPTR String
  deriving (Show)

instance Storable Answer where
  sizeOf _    = #{size adns_answer}
  alignment _ = alignment (undefined :: CInt)
  poke _ _    = error "poke is not defined for ADNS.Answer"
  peek ptr    = do
    sc <- #{peek adns_answer, status} ptr
    cn <- #{peek adns_answer, cname} ptr >>= maybePeek peekCString
    ow <- #{peek adns_answer, owner} ptr >>= maybePeek peekCString
    et <- #{peek adns_answer, expires} ptr
    rt <- #{peek adns_answer, type} ptr
    (rs :: CInt) <- #{peek adns_answer, nrrs} ptr
    (sz :: CInt) <- #{peek adns_answer, rrsz} ptr
    rrsp <- #{peek adns_answer, rrs} ptr
    r <- peekResp rt rrsp (fromEnum sz) (fromEnum rs)
    return Answer
        { status  = StatusCode sc
        , cname   = cn
        , owner   = ow
        , expires = et
        , rrs     = r
        }

-- |This function parses a the 'Response' union in the 'Answer'. It
-- cannot be defined via 'Storable', because it needs the type of the
-- record to expect as an additional parameter.
--
-- This is, by the way, the function to look at, if you want to add
-- support for additional 'RRType' records.

peekResp :: RRType -> Ptr b -> Int -> Int -> IO [Response]
peekResp _ _ _ 0      = return []
peekResp rt ptr off n = do
  r <- parseByType rt
  rs <- peekResp rt (ptr `plusPtr` off) off (n-1)
  return (r:rs)

  where
  parseByType A   = peek (castPtr ptr) >>= return . RRA . RRAddr
  parseByType NS  = peek (castPtr ptr) >>= return . RRNS
  parseByType PTR = peek (castPtr ptr) >>= peekCString >>= return . RRPTR
  parseByType MX  = do (RRIntHostAddr i addr) <- peek (castPtr ptr)
                       return (RRMX i addr)

----- Provide Wrappers for ADNS functions ----------------------------

-- |Internel helper function to handle result passing from ADNS via
-- @Ptr (Ptr a)@, and to generate human-readable IO exceptions in case
-- of an error.

wrapAdns :: (Ptr (Ptr b) -> IO CInt) -> (Ptr (Ptr b) -> IO a) -> IO a
wrapAdns m acc  = alloca $ \resP -> do
  poke resP nullPtr
  rc <- m resP
  if (rc == 0)
     then acc resP
     else do p <- adns_strerror rc
             s <- peekCString p
             fail ("ADNS: " ++ s)

-- |Map a list of flags ('Enum' types) into a 'CInt'
-- suitable for adns calls.

mkFlags :: Enum a => [a] -> CInt
mkFlags = toEnum . sum . map fromEnum

-- |Initialize the resolver. As of now, the diagnose stream
-- is always set to @stderr@. Initialize the library with
-- the 'NoErrPrint' flag if you don't wont to see any error
-- output there. All resources are freed automatically when
-- the 'State' falls out of scope.

adnsInit :: [InitFlag] -> IO State
adnsInit flags =
  wrapAdns
    (\p -> adns_init p (mkFlags flags) nullPtr)
    (\p -> peek p >>= newForeignPtr adnsFinalizer)

-- |Similar to 'adnsInit', but reads the resolver configuration from a
-- string rather than from @\/etc\/resolv.conf@. Supported are the
-- usual commands: @nameserver@, @search@, @domain@, @sortlist@, and
-- @options@.
--
-- Furthermore, these non-standard commands may be used:
--
--  * @clearnameservers@: Clears the list of nameservers, so that
--    further nameserver lines start again from the beginning.
--
--  * @include filename@: The specified file will be read.
--
-- For a full description, see the comments in the @adns.h@ header
-- file.

adnsInitCfg :: [InitFlag] -> String -> IO State
adnsInitCfg flags cfg = do
  cstr <- newCString cfg
  wrapAdns
      (\p -> adns_init_strcfg p (mkFlags flags) nullPtr cstr)
      (\p -> peek p >>= newForeignPtr adnsFinalizer)
    `finally` free cstr

-- |@adnsSynch st name rt flags@ performs a synchronous query for
-- record @name@ of type @rt@. @st@ must be a 'State' returned by
-- 'adnsInit'. @flags@ may be any of number of 'QueryFlag'.
--
-- In case of an I\/O error, an 'IOException' is thrown. If the query
-- fails for other reasons, the 'Status' code in the 'Answer' will
-- signify that.

adnsSynch :: State -> String -> RRType -> [QueryFlag] -> IO Answer
adnsSynch st' own rrt flags = withForeignPtr st' $ \st -> do
  o <- newCString own
  let rrt' = (toEnum . fromEnum) rrt
  wrapAdns
      (adns_synchronous st o rrt' (mkFlags flags))
      (\p -> peek p >>= peek)
   `finally` free o

-- |Submit an asynchronous query using the same parameters as
-- 'adnsSynch'. The returned 'Query' can be tested for completion with
-- 'adnsCheck'.

adnsSubmit :: State -> String -> RRType -> [QueryFlag] -> IO Query
adnsSubmit st' own rrt flags = withForeignPtr st' $ \st -> do
  o <- newCString own
  let rrt' = (toEnum . fromEnum) rrt
  wrapAdns
      (adns_submit st o rrt' (mkFlags flags) nullPtr)
      (peek)
    `finally` free o

-- |Check the status of an asynchronous query. If the query is
-- completed, the 'Answer' will be returned. The 'Query' becomes
-- invalid after that.

adnsCheck :: State -> Query -> IO (Maybe Answer)
adnsCheck st' q =
  withForeignPtr st' $ \st ->
    alloca $ \qPtr ->
      alloca $ \aPtr -> do
        poke qPtr q
        poke aPtr nullPtr
        rc <- adns_check st qPtr aPtr nullPtr
        case rc of
          0               -> peek aPtr >>= peek >>= return . Just
          #{const EAGAIN} -> return Nothing
          _               -> do p <- adns_strerror rc
                                s <- peekCString p
                                fail ("adnsCheck: " ++ s)

-- |Cancel an outstanding 'Query'.

foreign import ccall unsafe "adns_cancel" adnsCancel :: Query -> IO ()

type Shutdown = RawState -> IO ()
foreign import ccall "wrapper"
  mkShutdown :: Shutdown -> IO (FunPtr Shutdown)

{-# NOINLINE adnsFinalizer #-}
adnsFinalizer :: FunPtr Shutdown
adnsFinalizer = unsafePerformIO (mkShutdown adns_finish)

-- |Return the list of all currently outstanding queries.

adnsQueries :: State -> IO [Query]
adnsQueries st' = withForeignPtr st' $ \st ->
                    adns_forallqueries_begin st >> walk st
    where
    walk st = do q <- adns_forallqueries_next st nullPtr
                 if (q /= nullPtr)
                    then walk st >>= return . ((:) q)
                    else return []

-- |Opaque data type, provided only so that we can define 'sizeOf' for
-- it, in order to be able to 'mallocArray' an array for @poll(2)@.

data Pollfd

instance Storable Pollfd where
    sizeOf _    = #{size struct pollfd}
    alignment _ = alignment (undefined :: CInt)
    poke _ _    = error "poke is not defined for ADNS.Pollfd"
    peek _      = error "peek is not defined for ADNS.Pollfd"


-- |Original definition:
--
-- >    int adns_beforepoll(
-- >            adns_state ads,
-- >            struct pollfd *fds, int *nfds_io,
-- >            int *timeout_io,
-- >            const struct timeval *now);
--
-- Finds out which file descriptors ADNS is interested in, and when it
-- would like to be able to time things out. This is in a form
-- suitable for use with @poll(2)@.
--
-- On entry, usually @fds@ should point to at least @*nfds_io@
-- structs. ADNS will fill up to that many structs will information
-- for @poll@, and record in @*nfds_io@ how many structs it filled. If
-- it wants to listen for more structs then @*nfds_io@ will be set to
-- the number required and 'adns_beforepoll' will return 'eRANGE'.
--
-- You may call 'adns_beforepoll' with @fds=='nullPtr'@ and
-- @*nfds_io==0@, in which case ADNS will fill in the number of fds
-- that it might be interested in in @*nfds_io@, and always return
-- either 0 (if it is not interested in any fds) or 'eRANGE' (if it
-- is).
--
-- /Note/ that (unless @now@ is 0) ADNS may acquire additional fds
-- from one call to the next, so you must put adns_beforepoll in a
-- loop, rather than assuming that the second call (with the buffer
-- size requested by the first) will not return 'eRANGE'.
--
-- ADNS only ever sets @POLLIN@, @POLLOUT@ and @POLLPRI@ in its pollfd
-- structs, and only ever looks at those bits. @POLLPRI@ is required
-- to detect TCP Urgent Data (which should not be used by a DNS
-- server) so that ADNS can know that the TCP stream is now useless.
--
-- In any case, @*timeout_io@ should be a timeout value as for
-- @poll(2)@, which ADNS will modify downwards as required. If the
-- caller does not plan to block then @*timeout_io@ should be 0 on
-- entry, or alternatively, @timeout_io@ may be 0.
--
-- 'adns_beforepoll' will return 0 on success, and will not fail for any
-- reason other than the fds buffer being too small (ERANGE).
--
-- This call will never actually do any I\/O.  If you supply the
-- current time it will not change the fds that ADNS is using or the
-- timeouts it wants.
--
-- In any case this call won't block.

adnsBeforePoll ::
  State -> Ptr Pollfd -> Ptr CInt -> Ptr CInt -> Ptr Timeval
  -> IO CInt
adnsBeforePoll st' fds n size to =
  withForeignPtr st' $ \st -> adns_beforepoll st fds n size to

-- |Original definition:
--
-- >    void adns_afterpoll(adns_state ads,
-- >            const struct pollfd *fds, int nfds,
-- >            const struct timeval *now);
--
-- Gives ADNS flow-of-control for a bit; intended for use after
-- @poll(2)@. @fds@ and @nfds@ should be the results from @poll()@.
-- @pollfd@ structs mentioning fds not belonging to adns will be
-- ignored.

adnsAfterPoll :: State -> Ptr Pollfd -> CInt -> Ptr Timeval -> IO ()
adnsAfterPoll st' fds n to =
  withForeignPtr st' $ \st -> adns_afterpoll st fds n to

-- |The system routine @poll(2)@:
--
-- >    int poll(struct pollfd *ufds,
-- >             unsigned int nfds,
-- >             int timeout);

foreign import ccall unsafe poll :: Ptr Pollfd -> CUInt -> CInt -> IO CInt

-- |ADNS's scheduling calls take the current time of the day as
-- parameter, to avoid making unnecessary system calls. Hence, I
-- provide a marshaled version of C's @struct timeval@, so that this
-- value can be provided.

data Timeval = Timeval CTime #{type suseconds_t}
data Timezone

instance Storable Timeval where
  sizeOf _    = #{size struct timeval}
  alignment _ = alignment (undefined :: CTime)
  poke ptr (Timeval t us)
              = do #{poke struct timeval, tv_sec} ptr t
                   #{poke struct timeval, tv_usec} ptr us
  peek ptr    = do t <- #{peek struct timeval, tv_sec} ptr
                   us <- #{peek struct timeval, tv_usec} ptr
                   return (Timeval t us)

-- |Write the current time of the day as a 'Timeval'. The time is
-- returned in local time, no time zone correction takes place.

getTimeOfDay :: Ptr Timeval -> IO ()
getTimeOfDay p  = do
  rc <- gettimeofday p nullPtr
  case rc of
    0 -> return ()
    _ -> throwErrno "ADNS.getTimeOfDay"

-- |Map a 'Status' code to a human-readable error
-- description. For example:
--
-- >    *ADNS> adnsStrerror sNXDOMAIN >>= print
-- >    "No such domain"
--
-- Use this function with great care: It will crash the
-- process when called with a status code ADNS doesn't know
-- about. So only use it to print values you got from the
-- resolver!

adnsStrerror :: Status -> IO String
adnsStrerror (StatusCode x) = do
  cstr <- (adns_strerror . toEnum . fromEnum) x
  assert (cstr /= nullPtr) (peekCString cstr)

-- |Map a 'Status' code to a short error name. Don't use
-- this function with to print a status code unless you have
-- obtained it from the resolver!

adnsErrAbbrev :: Status -> IO String
adnsErrAbbrev (StatusCode x) = do
  cstr <- (adns_errabbrev . toEnum . fromEnum) x
  assert (cstr /= nullPtr) (peekCString cstr)

-- |Map a 'Status' code to a short description of the type
-- of error. Don't use this function with to print a status
-- code unless you have obtained it from the resolver!

adnsErrTypeAbbrev :: Status -> IO String
adnsErrTypeAbbrev (StatusCode x) = do
  cstr <- (adns_errtypeabbrev . toEnum . fromEnum) x
  assert (cstr /= nullPtr) (peekCString cstr)

----- Low-level C Functions ------------------------------------------

foreign import ccall unsafe adns_init ::
  Ptr RawState -> CInt -> Ptr CFile -> IO CInt

foreign import ccall unsafe adns_init_strcfg ::
  Ptr RawState -> CInt -> Ptr CFile -> CString-> IO CInt

foreign import ccall unsafe adns_finish ::
  RawState -> IO ()

foreign import ccall unsafe adns_submit ::
  RawState -> CString -> CInt -> CInt -> Ptr a -> Ptr Query
  -> IO CInt

foreign import ccall unsafe adns_check ::
  RawState -> Ptr Query -> Ptr (Ptr Answer) -> Ptr (Ptr a)
  -> IO CInt

foreign import ccall unsafe adns_synchronous ::
  RawState -> CString -> CInt -> CInt -> Ptr (Ptr Answer)
  -> IO CInt

foreign import ccall unsafe adns_beforepoll ::
  RawState -> Ptr Pollfd -> Ptr CInt -> Ptr CInt -> Ptr Timeval
  -> IO CInt

foreign import ccall unsafe adns_afterpoll ::
  RawState -> Ptr Pollfd -> CInt -> Ptr Timeval -> IO ()

foreign import ccall unsafe adns_forallqueries_begin ::
  RawState -> IO ()

foreign import ccall unsafe adns_forallqueries_next ::
  RawState -> Ptr (Ptr a) -> IO Query

foreign import ccall unsafe adns_strerror      :: CInt -> IO CString
foreign import ccall unsafe adns_errabbrev     :: CInt -> IO CString
foreign import ccall unsafe adns_errtypeabbrev :: CInt -> IO CString

foreign import ccall unsafe gettimeofday ::
  Ptr Timeval -> Ptr Timezone -> IO CInt

----- Helper Functions -----------------------------------------------

-- |Split up an IP address in network byte-order.

ha2tpl :: HostAddress -> (Int, Int, Int, Int)
ha2tpl n =
  let (b1,n1) = (n  .&. 255, n  `shiftR` 8)
      (b2,n2) = (n1 .&. 255, n1 `shiftR` 8)
      (b3,n3) = (n2 .&. 255, n2 `shiftR` 8)
      b4      = n3 .&. 255
  in
  case ourEndian of
    BigEndian    -> (fromEnum b4, fromEnum b3, fromEnum b2, fromEnum b1)
    LittleEndian -> (fromEnum b1, fromEnum b2, fromEnum b3, fromEnum b4)
    PDPEndian    -> (fromEnum b4, fromEnum b3, fromEnum b1, fromEnum b2)

-- |Turn a 32-bit IP address into a string suitable for PTR
-- lookups.

ha2ptr :: HostAddress -> String
ha2ptr n = shows b4 . ('.':) .
           shows b3 . ('.':) .
           shows b2 . ('.':) .
           shows b1 $ ".in-addr.arpa."
  where
  (b1,b2,b3,b4) = ha2tpl n


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" ) ***
-- End: ***
