## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

Thank you again for the quick turnaround and helpful comments. We addressed each of the comments by implementing the following changes:

 - Reset changes to user's options after each change to `par()` using `on.exit()` with the old settings
 - Make outputs to console easily suppressable by using `message()` or making them conditional on an argument
   - Note: We decided to keep one call to `cat()` in `api.R`, since it is conditional on the `log_to_console` argument and therefore easy to suppress. It could also be surprising to users if output would be suppressed despite `log_to_console` being explicitly set to `TRUE`. We could also easily 
