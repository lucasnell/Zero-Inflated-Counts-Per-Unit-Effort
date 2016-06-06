# ziCPUE:  Zero Inflated Catch Per Unit Effort

This repository is structured with three types of files for each step of the workflow:

- `*.html`: Instructive files with full code syntax highlighting, plots, etc. These files
  should serve as the primary files to view for instructions.
- `*.R`: Raw code intended for testing and implementing code.
- `*.Rmd`: Full documentation written in R Markdown files. These files contain little 
  code (most code is referenced from the `*.R` files) and were written to organize code
  and notes, and ultimately create the html files.

For instructions on external referencing in R Markdown and `knitr` (i.e., how I 
used chunks of code from separate R files in R markdown files), see 
[this guide](
http://zevross.com/blog/2014/07/09/making-use-of-external-r-code-in-knitr-and-r-markdown/
).

To see rendered versions of html files, prepend `"https://htmlpreview.github.io/?"`
to its original URL. The `compiling` html file would thus be:
```
https://htmlpreview.github.io/?https://github.com/lucasnell/ziCPUE/blob/master/compiling.html
```
