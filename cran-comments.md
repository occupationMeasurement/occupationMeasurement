## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

Thank you for the quick turnaround and helpful comments. We addressed each of the comments by implementing the following changes:

 - Moved authors out of brackets in the Description field in DESCRIPTION
 - Removed examples for non-exported functions
 - Updated functions to not store data outside of temp directories by default
 - Removed any global changes to `options()`
