# HOQCutil

## HOQCutil 0.1.25

* 20mar2021   
   + `hugo_plot_hook` is a knitr plot hook for use in the Hugo environment

## HOQCutil 0.1.24

* 28sep2020   
   + `capture.output.both` added that is an extension of `util::capture.output` and captures output, messages or both

## HOQCutil 0.1.23

* 12may2020   
   + `get_table_cbs_odata4` default for parameter `odata_root` adapted for new CBS (out of beta) environment 

## HOQCutil 0.1.22

* 12may2020   
   + `get_table_cbs_odata4` and `get_table_cbs_odata4_GET` also produce JSON output
   + `ref_tab` now has `def_text` parameter for the non-Latex case

## HOQCutil 0.1.21

* 09may2020   
   + `get_table_cbs_odata4` default for parameter `odata_root` adapted for new CBS beta test

## HOQCutil 0.1.20

* 05may2020   
   + `igs` added that generates insert of graphical statement

## HOQCutil 0.1.19

* 11mar2020   
   + `silent_library` now accepts character vector with package names

## HOQCutil 0.1.18

* 19feb2020   
   + `mfe_lookup` added

## HOQCutil 0.1.17

* 14feb2020   
   + `silent_library` now accepts expression for package name

## HOQCutil 0.1.16

* 30jan2020   
   + added functions  `set_fun_env` and `replace_package_fun` 

## HOQCutil 0.1.15

* 06dec2019   
   + added scanner functions `cleanup_bw`, `scan_with_hocr` and `extract_table` 

## HOQCutil 0.1.14

* 25oct2019   
   + added function `silent_library`

## HOQCutil 0.1.13

* 04oct2019   
   + added function `vassign` and `%va%`

## HOQCutil 0.1.12

* 27sep2019   
   + added function `debug_httr_get`
 
## HOQCutil 0.1.11

* 22sep2019   
   + added `line_numbering` option to `cap.out`
      
## HOQCutil 0.1.10

* 18mar2019   
   + added create_child_chunk and readLines_part
       
## HOQCutil 0.1.9

* 20dec2018   
   + added plot_PCA, plot_PCA_sel and print_PCA
   
## HOQCutil 0.1.8

* 12oct2018   
   + added pxtable: print.xtable with integrated `add.to.row` code
   
## HOQCutil 0.1.7

* 03oct2018   
   + get_table_cbs_odata4 no longer generates `$format=json` (the default)
   + get_table_cbs_odata4 parameter `error_msg` now default `TRUE`
   + get_table_cbs_odata4_GET reorganized to indicate json errors
   + added display_wrapped
  
## HOQCutil 0.1.6

* 02oct2018
   + get_table_cbs_odata4 shows extended error message with `error_msg = TRUE`

## HOQCutil 0.1.5

* 01oct2018
   + get_table_cbs_odata4 query `(id)` now returns data.frame

## HOQCutil 0.1.4

* 27sep2018
   + get_table_cbs_odata4 can handle queries `$count` and `(id)`
   + get_table_cbs_odata4_GET can handle xml and text data

## HOQCutil 0.1.3

* 16sep2018
   + get_table_cbs_odata4 and get_table_cbs_odata4_GET added

## HOQCutil 0.1.2

* 26aug2018
   + hard_split exported

## HOQCutil 0.1.1

* 13aug2018
   + leave out extra space in ref_tab when prefix shows as blanks
   + included the function cap.out

## HOQCutil 0.1

* 15jul2018 
   + Added def_tab and ref_tab to define labels and referring to them
   + Added format_WE and format_NS to format (WE) longitudes and (NS) lattitudes

## HOQCutil 0.0.0.9000

* 09jul2018 
   + Initialized package with myknit (only).
     
