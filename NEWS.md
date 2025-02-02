# ParamHelpers 1.14.2

- Fix documentation.
- Compatibility for new irace version
- Fix use of deprecated ggplot2 functionality

# ParamHelpers 1.14

- Fixes to work with R-4.0.0

# ParamHelpers 1.13

- `trafo` allowed in DiscreteVectorParam.
- Removed non-descriptive warning when a param has `NA` set as default.
- `generateDesign` rewritten in `R` to avoid random error in C `REAL() can only be applied to a 'numeric', not a 'NULL'`. Function should only be slower if requirements are given in a non vectorized manner.

# ParamHelpers 1.12

- improved documentation
- speedups

# ParamHelpers 1.10

- adapt to irace 2.1 API changes

## new functions

- getRequirements
- isSpecialValue
- getTypeStringsLogical, getTypeStringsCharacter, getTypeStringsAll
- isNumericTypeString, isIntegerTypeString, isDiscreteTypeString, isLogicalTypeString, isCharacterTypeString, isVectorTypeString
- generateDesignOfDefaults

# ParamHelpers 1.9

- Many parameters can now be expressions which can be evaluated later while providing a lookup dictionary.
- make*Param now accepts special.vals which are always feasible
- getParamLengths, getLower, getUpper, getValues were made S3 to also work on normal Parameter objects

## new functions

- evaluateParamExpressions
- hasExpression

# ParamHelpers 1.8

- plots produced by renderOptPathPlot, plotOptPath are now always nicely aligned
- removed soobench from SUGGESTS and removed function extractParamSetFromSooFunction
- hasDiscrete: added arg "include.logical"
- getParamSet has become a generic
- OptPath: it is possible to log non-scalar values under "extras" now by starting the extra-name with a dot.
  These elements wll not be contained is as.data.frame(opt.path) when converted.
- make*Param argument 'requires' now accepts expressions and quote'd expressions.
- getParamIds is now S3 and also callable on type "Param"
- filterParams: has new arg "check.requires"
- isFeasible: new args "use.defaults" and "filter"
- trafoValue: slightly robustify arg handling

## new functions

- isNumericStrict
- getTypeStringsNumeric, getTypeStringsNumericStrict, getTypeStringsInteger, getTypeStringsDiscrete
- filterParamsNumeric, filterParamsDiscrete
- updateParVals

# ParamHelpers 1.7

- dfRowsToList, dfRowToList: added arg enforce.col.types
- UntypedParam also got the "tunable" flag

# ParamHelpers 1.6

- added new character parameter types, see new functions below
- added property 'tunable' to make*Param
- isFeasible supports named lists for unordered checks
- numeric and numericvector Params and LearnerParams have a new arg 'allow.inf'
- filterParams can now filter based on 'ids' and 'tunable'
- convertParamSetToIrace: removed argument 'digits' to keep maximum precision of boundaries
- added renderOptPathPlot and plotOptPath for plotting of optimization paths.
- added plotEAF and plotYTraces to compare singlecrit and bicrit optimization paths.

## new functions

- isRequiresOk
- plotEAF
- plotYTraces
- renderOptPathPlot, plotOptPath
- makeCharacterParam, makeCharacterVectorParam
- isCharacter, hasCharacter

# ParamHelpers 1.5

- added constr.clip to print.ParamSet and related internal functions to make prints more readable
- getDefaults: new option include.null

# ParamHelpers 1.4

- sampleValue(s): 'discrete.names' now only affects discrtete params and not logicals anymore.
  logical params always result in R data type 'logical'
- Vector params (num, int and log) can now have component names
- many more OptPath getter methods allow the selection option 'dob' and 'eol' to subset the result
## new functions
- getOptPathX
- generateRandomDesign

# ParamHelpers 1.3

- sampleValue(s): 'discrete.names' now only affects discrtete params and not logicals anymore.
  logical params always result in R data type 'logical'
- Vector params (num, int and log) can now have component names Functions that produce such vector values (e.g. sampleValue or dfRowsToList)
  will name the components of this vector automatically.
  The user can also call setValueCNames in his own code.

## new functions
- getOptPathCols
- setValueCNames
- repairPoint

# ParamHelpers 1.2

- normal parameters can now have a default value
- parameter spaces can now define a forbidden region of infeasibility.
  all operations on the set respect this, although a few are disallowed and produce an exception.
- rewrote generateDesign and dfRowsToList in C to greatly increase speed in the case of dependent parameters
- generateDesign and generateGridDesign do not support type conversion anymore. See help page.
- generateDesign now ensures that no duplicted settings are produced in rarer cases
- Added option to log error messages, execution time and extra info in the optimization path.
- slightly change interface of getOptPathY, so one can now return a matrix of multiple measures.
- paramValueToString produces better readable output
- sampleValue(s) have new option "trafo"
- getLower and getUpper have with.nr argument now
- getTypes renamed to getParamTypes

## new functions
- isNumeric, isDiscrete, isInteger, isLogical
- hasNumeric, hasInteger, hasDiscrete, hasLogical
- getParamNr
- getDefaults
- hasTrafo, hasRequires
- isForbidden, hasForbidden
- generateGridDesign
- getOptPathExecTimes, getOptPathErrorMessages
- getOptPathParetoFront
- getOptPathCol

# ParamHelpers 1.1-35

- concept of dependent parameters was introduced (argument "requires")
- as.data.frame.OptPath: discretes.as.factor option
- all arguments "length" renamed to len

## new functions
- convertParamSetToIrace
- dfRowToList, dfRowsToList
- getOptPathDOB, getOptPathEOL
- hasRequires
- removeMissingValues

# ParamHelpers 1.0-55

- disabled one unreasonable unit test for CRAN
- some dependencies are now imports like they should be

# ParamHelpers 1.0-54

- First submit to CRAN.
