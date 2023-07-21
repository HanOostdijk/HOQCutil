# HOQCutil
This package groups some utilities that I often use.

Currently the package includes 

* function `myknit` used in the 
[**rmd_pdf_examples repository**](https://github.com/HanOostdijk/rmd_pdf) in example `flex_knit`
* functions `def_tab` and `ref_tab` to define and reference labels in a LaTeX document. Examples of use can be found in the repository mentioned above.
* functions `format_WE` and `format_NS` to format longitudes and latitudes
* function `cap.out` that captures and wraps output so that they can be included in Rmd documents
* function `capture.output.both` that captures output, messages or both (used by `cap.out`)
* function `hard_split` that splits strings at certain positions
* function `display_wrapped` that show split strings in the console
* function `get_table_cbs_odata4` (and `get_table_cbs_odata4_GET`) used for access to CBS OData4 data
* function `pxtable` that calls print.xtable with integrated `add.to.row` code and behaves as `kable` when html output is produced
* functions `plot_PCA` and `print_PCA1` that plot and print `FactoMineR::PCA` results
* functions `create_child_chunk` and `readLines_part` that can be used to read a part of a text file and create a child chunk that can be used in a `.Rmd` document
* function `debug_httr_plot` that debugs `httr::GET` by setting a temporarian trace and retrieving its input and output
* function `vassign` and derived binary operator `%va%` that allow for assigning multiple variables in one call
* function `silent_library` to attach a library with as few messages as possible
* functions `cleanup_bw`, `scan_with_hocr` and `extract_table` to cleanup and scan (OCR) an image and extract a table into a data.frame format
* functions `replace_package_fun` and `set_fun_env` that can be used to (temporarily) replace a function in a package 
* function `mfe_lookup` that can be used to lookup strings e.g. to categorize transactions
* function `igs` that can be used to insert an html statement to include an image in a web document
* function `hugo_plot_hook` is a knitr plot hook for use in the Hugo environment.
* functions `read_pdf`, `read_pdf_line` , `read_pdf_cut`, `read_pdf_fields` and `cut3d` are used for reading PDF text data
* function `text2pdf` to write a character vector to a pdf file

## Install HOQCutil

```R
devtools::install_github("HanOostdijk/HOQCutil") 
```
