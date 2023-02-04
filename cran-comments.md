## R CMD check results  

There were no ERRORs , NOTEs or WARNINGs. 

## Downstream dependencies  

There are currently no downstream dependencies for this package.  

## Notes  

**1. (problem)** " \\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \\dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
Please replace \\dontrun with \\donttest.
All your examples are wrapped in \\dontrun{} and therefore do not get tested.
Please unwrap the examples if that is feasible and if they can be
executed in < 5 sec for each Rd file or create additionally small toy
examples to allow automatic testing.
Please put functions which download data in \\donttest{}."

**1. (response)** The package relies on an API, which requires a connection to the internet. In addition, the `@examples` use parallel processing in some cases. For this reason I think `\dontrun{}` is the best option for `@examples` with API calls and parallel processing examples.
