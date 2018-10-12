# HOQCutil
This package groups some utilities that I often use.

Currently the package includes 

* function `myknit` used in the 
[**rmd_pdf_examples repository**](https://github.com/HanOostdijk/rmd_pdf) in example `flex_knit`
* functions `def_tab` and `ref_tab` to define and reference labels in a LaTeX document. Examples of use can be found in the repository mentioned above.
* functions `format_WE` and `format_NS` to format longitudes and latitudes
* function `cap.out` that captures and wraps output so that they can be included in Rmd documents
* function `hard_split` that splits strings at certain positions
* function `display_wrapped` that show split strings in the console
* function `get_table_cbs_odata4` (and `get_table_cbs_odata4_GET`) used for access to CBS OData4 data
* function `pxtable` that calls print.xtable with integrated `add.to.row` code


## Install HOQCutil

```R
install.packages("devtools")  
library(devtools)  
install_github("HanOostdijk/HOQCutil") 
```
