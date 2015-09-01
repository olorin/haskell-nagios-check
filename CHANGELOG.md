# Changelog

## v0.3.2

 - Deprecate `UnknownUOM`.

## v0.3.1

 - Fix bug in perfdata rendering wherein scientific notation was used
   for large/small values.

## v0.3.0

 - Added addPerfData function and ToPerfData typeclass for more convenient
   generation of perfdata from complex data structures.

## v0.2.1

 - Support for GHC 7.4 (Justin S. Leitgeib, @jsl).
 - Fix bug in perfdata ordering (correct order is warn, crit, min, max;
   not min, max, warn, crit).

## v0.2.0

 - add addBarePerfdatum convenience function
