# conquestr 0.1.2.9000

* `cq_show` has been added to import item parameters and standard errors (also
relates to #20)
* `cq_itanal` is now more flexible and handles different flavours of ConQuest v2 
output as well as when fit statistics are not included in the output. (#19, #20)
* `cqc_resp_cols` added to provide condensed item column specifications for the
ConQuest format statement.

# conquestr 0.1.2

* Added `cqc_syntax` command for generating ConQuest syntax.  Commands currently supported are in `cqc_cmds`. Default arguments for commands are in `cqc_defaults`.

# conquestr 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Import unidimensional plausible values files with `cq_pv`
* `cq_example_itanal` is now deprecated. Use `cq_example` instead which now includes options for retrieving plausible values files as well.

# conquestr 0.1.0

* Initial release
* Import item analysis files with `cq_itanal`
