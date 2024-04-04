This package is intended for importing PIMSoft reports in XML format as single row observations into a R dataframe and can also be used on a folder of XML files generated from PIMSoft.

It is based on xml2 and tidyverse packages.

It includes two functions "xml_to_singlerow" and "singlerow_df_compile".

"xml_to_singlerow" takes a xml file path and generates a single row tibble df. "singlerow_df_compile" takes a file path to a folder of XML files and compiles all of them into a data frame.
At this moment, it only takes reports generated with "Recording info" and "Mean perfusion" boxes ticked.
