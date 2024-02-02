-- editorconfig-checker-disable
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import PlutusCore qualified as PLC
import PlutusCore.Annotation (SrcSpan)
import PlutusCore.Data (Data)
import PlutusCore.Default (BuiltinSemanticsVariant (..))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..), ExRestrictingBudget (..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusCore.Executable.Common
import PlutusCore.Executable.Parsers
import PlutusCore.MkPlc (mkConstant)
import PlutusPrelude

import PlutusCore.Evaluation.Machine.MachineParameters
import UntypedPlutusCore.Evaluation.Machine.SteppableCek.DebugDriver qualified as D
import UntypedPlutusCore.Evaluation.Machine.SteppableCek.Internal qualified as D

import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (FreeVariableError)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

import Control.DeepSeq (force)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Criterion (benchmarkWith, whnf)
import Criterion.Main (defaultConfig)
import Criterion.Types (Config (..))
import Data.ByteString.Lazy as BSL (readFile)
import Data.Foldable (asum)
import Data.List.Split (splitOn)
import Data.Text qualified as T
import Flat (unflat)
import Options.Applicative
import Prettyprinter ((<+>))
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import Text.Read (readMaybe)

import Control.Monad.ST (RealWorld)
import System.Console.Haskeline qualified as Repl

-- Extra imports for experimental benchmarking code
import Codec.Serialise (Serialise)
import Control.Exception qualified as E
import Control.Monad.Except (MonadError)
import Data.ByteString qualified as BS
import NoThunks.Class
import PlutusCore.Default qualified as PLC (BuiltinSemanticsVariant (DefaultFunSemanticsVariant1))
import PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import PlutusCore.Evaluation.Machine.ExBudget as Plutus
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as Plutus
import PlutusCore.Evaluation.Machine.MachineParameters.Default

uplcHelpText :: String
uplcHelpText = helpText "Untyped Plutus Core"

uplcInfoCommand :: ParserInfo Command
uplcInfoCommand = plutus uplcHelpText

data BudgetMode  = Silent
                 | Verbose SomeBudgetMode

data SomeBudgetMode =
    forall cost. (Eq cost, NFData cost, PrintBudgetState cost) =>
        SomeBudgetMode (Cek.ExBudgetMode cost PLC.DefaultUni PLC.DefaultFun)

data EvalOptions =
    EvalOptions
      Input
      Format
      PrintMode
      BudgetMode
      TraceMode
      Output
      CekModel
      (BuiltinSemanticsVariant PLC.DefaultFun)

data BenchmarkOptions =
    BenchmarkOptions
      Input
      Format
      (BuiltinSemanticsVariant PLC.DefaultFun)
      Double

data DbgOptions =
    DbgOptions Input Format CekModel

---------------- Main commands -----------------

data Command = Apply       ApplyOptions
             | ApplyToData ApplyOptions
             | Benchmark   BenchmarkOptions
             | Convert     ConvertOptions
             | Optimise    OptimiseOptions
             | Print       PrintOptions
             | Example     ExampleOptions
             | Eval        EvalOptions
             | Dbg         DbgOptions
             | DumpModel
             | PrintBuiltinSignatures

---------------- Option parsers ----------------

cekmodel :: Parser CekModel
cekmodel =
    flag Default Unit
        (  short '1'
        <> long "unit-cek-model"
        <> help "Use unit AST node costs and builtin costs for CEK cost model (tallying mode only)"
        )

benchmarkOpts :: Parser BenchmarkOptions
benchmarkOpts =
  BenchmarkOptions
  <$> input
  <*> inputformat
  <*> builtinSemanticsVariant
  <*> option auto
          (  long "time-limit"
          <> short 'T'
          <> metavar "TIME LIMIT"
          <> value 5.0
          <> showDefault
          <> help "Time limit (in seconds) for benchmarking.")

evalOpts :: Parser EvalOptions
evalOpts =
  EvalOptions
  <$> input
  <*> inputformat
  <*> printmode
  <*> budgetmode
  <*> tracemode
  <*> output
  <*> cekmodel
  <*> builtinSemanticsVariant

dbgOpts :: Parser DbgOptions
dbgOpts =
  DbgOptions <$>
    input <*> inputformat <*> cekmodel

-- Reader for budget.  The --restricting option requires two integer arguments
-- and the easiest way to do this is to supply a colon-separated pair of
-- integers.
exbudgetReader :: ReadM ExBudget
exbudgetReader = do
  s <- str
  case splitOn ":" s of
    [a,b] -> case (readMaybe a, readMaybe b) of
               (Just cpu, Just mem) -> pure $ ExBudget (ExCPU cpu) (ExMemory mem)
               _                    -> readerError badfmt
    _     -> readerError badfmt
    where badfmt = "Invalid budget (expected eg 10000:50000)"

restrictingbudgetEnormous :: Parser BudgetMode
restrictingbudgetEnormous =
    flag' (Verbose $ SomeBudgetMode Cek.restrictingEnormous)
        (  long "restricting-enormous"
        <> short 'r'
        <> help "Run the machine in restricting mode with an enormous budget" )

restrictingbudget :: Parser BudgetMode
restrictingbudget =
    Verbose . SomeBudgetMode . Cek.restricting . ExRestrictingBudget
        <$> option exbudgetReader
                (  long "restricting"
                <> short 'R'
                <> metavar "ExCPU:ExMemory"
                <> help "Run the machine in restricting mode with the given limits" )

countingbudget :: Parser BudgetMode
countingbudget = flag' (Verbose $ SomeBudgetMode Cek.counting)
                 (  long "counting"
                 <> short 'c'
                 <> help "Run machine in counting mode and report results" )

tallyingbudget :: Parser BudgetMode
tallyingbudget = flag' (Verbose $ SomeBudgetMode Cek.tallying)
                 (  long "tallying"
                 <> short 't'
                 <> help "Run machine in tallying mode and report results" )

budgetmode :: Parser BudgetMode
budgetmode = asum
    [ restrictingbudgetEnormous
    , restrictingbudget
    , countingbudget
    , tallyingbudget
    , pure Silent
    ]

plutus ::
  -- | The @helpText@
  String ->
  ParserInfo Command
plutus langHelpText =
    info
      (plutusOpts <**> helper)
      (fullDesc <> header "Untyped Plutus Core Tool" <> progDesc langHelpText)

plutusOpts :: Parser Command
plutusOpts = hsubparser $
       command "apply"
           (info (Apply <$> applyOpts)
            (progDesc $ "Given a list of input files f g1 g2 ... gn " <>
             "containing Untyped Plutus Core scripts, " <>
             "output a script consisting of (... ((f g1) g2) ... gn); " <>
             "for example, 'uplc apply --if flat Validator.flat " <>
             "Datum.flat Redeemer.flat Context.flat --of flat -o Script.flat'."))
    <> command "apply-to-data"
           (info (ApplyToData <$> applyOpts)
            (progDesc $ "Given a list f d1 d2 ... dn where f is an " <>
             "Untyped Plutus Core script and d1,...,dn are files " <>
             "containing flat-encoded data ojbects, output a script " <>
             "consisting of f applied to the data objects; " <>
             "for example, 'uplc apply-to-data --if " <>
             "flat Validator.flat Datum.flat Redeemer.flat Context.flat " <>
             "--of flat -o Script.flat'."))
    <> command "print"
           (info (Print <$> printOpts)
            (progDesc "Parse a program then prettyprint it."))
    <> command "convert"
           (info (Convert <$> convertOpts)
            (progDesc "Convert a program between various formats."))
    <> command "optimise" (optimise "Run the UPLC optimisation pipeline on the input.")
    <> command "optimize" (optimise "Same as 'optimise'.")
    <> command "example"
           (info (Example <$> exampleOpts)
            (progDesc $ "Show a program example. "
                     ++ "Usage: first request the list of available examples (optional step), "
                     ++ "then request a particular example by the name of a term. "
                     ++ "Note that evaluating a generated example may result in 'Failure'."))
    <> command "benchmark"
           (info (Benchmark <$> benchmarkOpts)
            (progDesc "Benchmark an untyped Plutus Core program on the CEK machine using Criterion."))
    <> command "evaluate"
           (info (Eval <$> evalOpts)
            (progDesc "Evaluate an untyped Plutus Core program using the CEK machine."))
    <> command "debug"
           (info (Dbg <$> dbgOpts)
            (progDesc "Debug an untyped Plutus Core program using the CEK machine."))
    <> command "dump-model"
           (info (pure DumpModel)
            (progDesc "Dump the cost model parameters."))
    <> command "print-builtin-signatures"
           (info (pure PrintBuiltinSignatures)
            (progDesc "Print the signatures of the built-in functions."))
    where optimise desc = info (Optimise <$> optimiseOpts) $ progDesc desc


---------------- Optimisation ----------------

-- | Run the UPLC optimisations
runOptimisations:: OptimiseOptions -> IO ()
runOptimisations (OptimiseOptions inp ifmt outp ofmt mode) = do
    prog <- readProgram ifmt inp :: IO (UplcProg SrcSpan)
    simplified <- PLC.runQuoteT $ do
                    renamed <- PLC.rename prog
                    UPLC.simplifyProgram UPLC.defaultSimplifyOpts renamed
    writeProgram outp ofmt mode simplified

---------------- Script application ----------------

-- | Apply one script to a list of others and output the result.  All of the
-- scripts must be UPLC.Program objects.
runApply :: ApplyOptions -> IO ()
runApply (ApplyOptions inputfiles ifmt outp ofmt mode) = do
  scripts <- mapM ((readProgram ifmt ::  Input -> IO (UplcProg SrcSpan)) . FileInput) inputfiles
  let appliedScript =
        case void <$> scripts of
          []          -> errorWithoutStackTrace "No input files"
          progAndargs ->
            foldl1 (unsafeFromRight .* UPLC.applyProgram) progAndargs
  writeProgram outp ofmt mode appliedScript

-- | Apply a UPLC program to script to a list of flat-encoded Data objects and
-- output the result.
runApplyToData :: ApplyOptions -> IO ()
runApplyToData (ApplyOptions inputfiles ifmt outp ofmt mode) =
  case inputfiles  of
    [] -> errorWithoutStackTrace "No input files"
    p:ds -> do
         prog@(UPLC.Program _ version _) :: UplcProg SrcSpan <- readProgram ifmt (FileInput p)
         args <- mapM (getDataObject version) ds
         let prog' = () <$ prog
             appliedScript = foldl1 (unsafeFromRight .* UPLC.applyProgram) (prog':args)
         writeProgram outp ofmt mode appliedScript
             where getDataObject :: UPLC.Version -> FilePath -> IO (UplcProg ())
                   getDataObject ver path = do
                     bs <- BSL.readFile path
                     case unflat bs of
                       Left err -> fail ("Error reading " ++ show path ++ ": " ++ show err)
                       Right (d :: Data) ->
                           pure $ UPLC.Program () ver $ mkConstant () d

---------------- Benchmarking ----------------

-- Lots of stuff copied out of plutus-ledger-api and plutus-benchmark to try to
-- reproduce the normal benchmarking behaviour.

newtype MajorProtocolVersion = MajorProtocolVersion { getMajorProtocolVersion :: Int }
  deriving newtype (Eq, Ord, Show, Serialise)
  deriving stock (Generic)

toMachineParameters :: MajorProtocolVersion -> EvaluationContext -> DefaultMachineParameters
toMachineParameters _ = machineParameters

newtype EvaluationContext = EvaluationContext
    { machineParameters :: DefaultMachineParameters
    }
    deriving stock Generic
    deriving anyclass (NFData, NoThunks)

data VerboseMode =
    Verbose2 -- ^ accumulate all traces
    | Quiet -- ^ don't accumulate anything
    deriving stock (Eq)


evaluateTerm
    :: Cek.ExBudgetMode cost UPLC.DefaultUni UPLC.DefaultFun
    -> MajorProtocolVersion
    -> VerboseMode
    -> EvaluationContext
    -> UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
    -> ( Either
            (Cek.CekEvaluationException UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun)
            (UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
       , cost
       , [T.Text]
       )

evaluateTerm budgetMode pv verbose ectx =
    Cek.runCekDeBruijn
        (toMachineParameters pv ectx)
        budgetMode
        (if verbose == Verbose2 then Cek.logEmitter else Cek.noEmitter)
-- Just replicating the old behavior, probably doesn't matter.
{-# INLINE evaluateTerm #-}

data PlutusLedgerLanguage =
      PlutusV1 -- ^ introduced in shelley era
    | PlutusV2 -- ^ introduced in vasil era
    | PlutusV3 -- ^ not yet enabled
   deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

alonzoPV :: MajorProtocolVersion
alonzoPV = MajorProtocolVersion 5

-- | Vasil era was introduced in protocol version 7.0
vasilPV :: MajorProtocolVersion
vasilPV = MajorProtocolVersion 7

-- | Conway era was introduced in protocol version 9.0
conwayPV :: MajorProtocolVersion
conwayPV = MajorProtocolVersion 9

ledgerLanguageIntroducedIn :: PlutusLedgerLanguage -> MajorProtocolVersion
ledgerLanguageIntroducedIn = \case
    PlutusV1 -> alonzoPV
    PlutusV2 -> vasilPV
    PlutusV3 -> conwayPV

evaluateCekLikeInProd
    :: EvaluationContext
    -> UPLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
    -> Either
            (Cek.CekEvaluationException UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun)
            (UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
evaluateCekLikeInProd evalCtx term = do
    let (getRes, _, _) =
            -- The validation benchmarks were all created from PlutusV1 scripts
            evaluateTerm Cek.restrictingEnormous (ledgerLanguageIntroducedIn PlutusV1) Quiet evalCtx term
    getRes

unsafeUnflat :: String -> BS.ByteString -> UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
unsafeUnflat file contents =
    case unflat contents of
        Left e     -> errorWithoutStackTrace $ "Flat deserialisation failure for " ++ file ++ ": " ++ show e
        Right (UPLC.UnrestrictedProgram prog) -> prog


toNamedDeBruijnTerm
    :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
    -> UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
toNamedDeBruijnTerm = UPLC.termMapNames UPLC.fakeNameDeBruijn

mkDynEvaluationContext
    :: MonadError CostModelApplyError m
    => BuiltinSemanticsVariant UPLC.DefaultFun
    -> Plutus.CostModelParams
    -> m EvaluationContext
mkDynEvaluationContext semvar newCMP =
    EvaluationContext <$> mkMachineParametersFor semvar newCMP


mkEvalCtx :: EvaluationContext
mkEvalCtx =
    case PLC.defaultCostModelParams of
        -- The validation benchmarks were all created from PlutusV1 scripts
        Just p -> case mkDynEvaluationContext PLC.DefaultFunSemanticsVariant1 p of
            Right ec -> ec
            Left err -> error $ show err
        Nothing -> error "Couldn't get cost model params"

-- The original version of the benchmarking function.
_runBenchmark :: BenchmarkOptions -> IO ()
_runBenchmark (BenchmarkOptions inp ifmt semvar timeLim) = do
  prog <- readProgram ifmt inp
  let criterionConfig = defaultConfig {reportFile = Nothing, timeLimit = timeLim}
      cekparams = mkMachineParameters semvar PLC.defaultCekCostModel
      getResult (x,_,_) = either (error . show) (\_ -> ()) x  -- Extract an evaluation result
      evaluate = getResult . Cek.runCekDeBruijn cekparams Cek.restrictingEnormous Cek.noEmitter
      -- readProgam throws away De Bruijn indices and returns an AST with Names;
      -- we have to put them back to get an AST with NamedDeBruijn names.
      !term = fromRight (error "Unexpected open term in runBenchmark.") .
                runExcept @FreeVariableError $ UPLC.deBruijnTerm (UPLC._progTerm prog)
      -- Big names slow things down
      !anonTerm = UPLC.termMapNames (\(PLC.NamedDeBruijn _ i) -> PLC.NamedDeBruijn "" i) term
      -- Big annotations slow things down
      !unitAnnTerm = force (() <$ anonTerm)
  benchmarkWith criterionConfig $! whnf evaluate unitAnnTerm

-- Try to do the same as plutus-benchmark
runBenchmark :: BenchmarkOptions -> IO ()
runBenchmark (BenchmarkOptions inp ifmt semvar timeLim) = do
  evalCtx <- E.evaluate $ force mkEvalCtx
  let file = case inp of
               FileInput path -> path
               _              -> error "Want a file"
  contents <- BS.readFile file
  let criterionConfig = defaultConfig {reportFile = Nothing, timeLimit = timeLim}
      -- readProgam throws away De Bruijn indices and returns an AST with Names;
      -- we have to put them back to get an AST with NamedDeBruijn names.
      mkCekBM  =
          -- don't count the undebruijn . unflat cost
          -- `force` to try to ensure that deserialiation is not included in benchmarking time.
          let !benchTerm = force . toNamedDeBruijnTerm . UPLC._progTerm $  unsafeUnflat file contents
              eval = either (error . show) (\_ -> ()) . evaluateCekLikeInProd evalCtx
          in whnf eval benchTerm
  benchmarkWith criterionConfig $ mkCekBM

---------------- Evaluation ----------------

runEval :: EvalOptions -> IO ()
runEval (EvalOptions inp ifmt printMode budgetMode traceMode
                     outputMode cekModel semvar) = do
    prog <- readProgram ifmt inp
    let term = void $ prog ^. UPLC.progTerm
        cekparams = case cekModel of
                    -- AST nodes are charged according to the default cost model
                    Default -> mkMachineParameters semvar PLC.defaultCekCostModel
                    -- AST nodes are charged one unit each, so we can see how many times each node
                    -- type is encountered.  This is useful for calibrating the budgeting code
                    Unit    -> PLC.unitCekParameters
    let emitM = case traceMode of
            None               -> Cek.noEmitter
            Logs               -> Cek.logEmitter
            LogsWithTimestamps -> Cek.logWithTimeEmitter
            LogsWithBudgets    -> Cek.logWithBudgetEmitter
    -- Need the existential cost type in scope
    let budgetM = case budgetMode of
            Silent     -> SomeBudgetMode Cek.restrictingEnormous
            Verbose bm -> bm
    case budgetM of
        SomeBudgetMode bm ->
            do
              let (res, budget, logs) = Cek.runCek cekparams bm emitM term
              case res of
                Left err -> hPrint stderr err
                Right v  -> writeToFileOrStd outputMode (show (getPrintMethod printMode v))
              case budgetMode of
                Silent    -> pure ()
                Verbose _ -> printBudgetState term cekModel budget
              case traceMode of
                None -> pure ()
                _    -> writeToFileOrStd outputMode (T.unpack (T.intercalate "\n" logs))
              case res of
                Left _  -> exitFailure
                Right _ -> pure ()

---------------- Debugging ----------------

runDbg :: DbgOptions -> IO ()
runDbg (DbgOptions inp ifmt cekModel) = do
    prog <- readProgram ifmt inp
    let term = prog ^. UPLC.progTerm
        nterm = fromRight (error "Term to debug must be closed.") $
                   runExcept @FreeVariableError $ UPLC.deBruijnTerm term
    let cekparams = case cekModel of
                    -- AST nodes are charged according to the default cost model
                    Default -> PLC.defaultCekParameters
                    -- AST nodes are charged one unit each, so we can see how many times each node
                    -- type is encountered.  This is useful for calibrating the budgeting code
                    Unit    -> PLC.unitCekParameters
        replSettings = Repl.Settings { Repl.complete = Repl.noCompletion
                                     , Repl.historyFile = Nothing
                                     , Repl.autoAddHistory = False
                                     }
    -- nilSlippage is important so as to get correct live up-to-date budget
    cekTrans <- fst <$> D.mkCekTrans cekparams Cek.restrictingEnormous Cek.noEmitter D.nilSlippage
    Repl.runInputT replSettings $
        -- MAYBE: use cutoff or partialIterT to prevent runaway
        D.iterTM (handleDbg cekTrans) $ D.runDriverT nterm

-- TODO: this is just an example of an optional single breakpoint, decide
-- if we actually want breakpoints for the cli
newtype MaybeBreakpoint = MaybeBreakpoint { _fromMaybeBreakpoint :: Maybe SrcSpan }
type DAnn = SrcSpan
instance D.Breakpointable DAnn MaybeBreakpoint where
    hasBreakpoints = error "Not implemented: Breakpointable DAnn Breakpoints"

-- Peel off one layer
handleDbg :: (Cek.ThrowableBuiltins uni fun)
          => D.CekTrans uni fun DAnn RealWorld
          -> D.DebugF uni fun DAnn MaybeBreakpoint (Repl.InputT IO ())
          -> Repl.InputT IO ()
handleDbg cekTrans = \case
    D.StepF prevState k  -> do
        -- Note that we first turn Cek to IO and then `liftIO` it to InputT; the alternative of
        -- directly using MonadTrans.lift needs MonadCatch+MonadMask instances for CekM, i.e. messy
        -- also liftIO would be unnecessary if haskeline.InputT worked with `primitive`
        eNewState <- liftIO $ D.liftCek $ tryError $ cekTrans prevState
        case eNewState of
            Right newState -> k newState
            Left e         -> Repl.outputStrLn $ show e
                             -- no kontinuation, so it acts like exitSuccess
                             -- FIXME: decide what should happen after the error occurs
    D.InputF k           -> handleInput >>= k
    D.DriverLogF text k        -> handleLog text >> k
    D.UpdateClientF ds k -> handleUpdate ds >> k
  where
    handleInput = do
        c <- Repl.getInputChar "(s)tep (c)ontinue (n)ext (f)inish (Ctrl+d exit):"
        -- TODO: implement print "program counter", breakpoints
        -- MAYBE: switch to repline
        case c of
            Just 's' -> pure D.Step
            Just 'c' -> pure $ D.Continue $ MaybeBreakpoint empty
            Just 'n' -> pure $ D.Next $ MaybeBreakpoint empty
            Just 'f' -> pure $ D.Finish $ MaybeBreakpoint empty
            -- otherwise retry
            _        -> handleInput
    handleUpdate s = Repl.outputStrLn $ show $ "Updated state:" <+> pretty s
    handleLog = Repl.outputStrLn . T.unpack

----------------- Print examples -----------------------
runUplcPrintExample ::
    ExampleOptions -> IO ()
runUplcPrintExample = runPrintExample getUplcExamples

---------------- Driver ----------------

main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) uplcInfoCommand
    case options of
        Apply       opts       -> runApply             opts
        ApplyToData opts       -> runApplyToData       opts
        Benchmark   opts       -> runBenchmark         opts
        Eval        opts       -> runEval              opts
        Dbg         opts       -> runDbg               opts
        Example     opts       -> runUplcPrintExample  opts
        Optimise    opts       -> runOptimisations     opts
        Print       opts       -> runPrint   @UplcProg opts
        Convert     opts       -> runConvert @UplcProg opts
        DumpModel              -> runDumpModel
        PrintBuiltinSignatures -> runPrintBuiltinSignatures
