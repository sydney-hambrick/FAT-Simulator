README
================

Inputs: excel file that contains all FAT simulation data

Outputs: excel file that contains all paired data

Summary: Create a new temp SQL table that uses the following logic:

“SELECT a.\*, b.\[all_value\] as b.\[all_value\]\_ref, FROM file as a,
file as b WHERE a.\[match_value\]=b.\[match_value\], a.\[cross_value\]
\<\> b.\[cross_value\]”

From here uses the input FAT criteria to determine Pass/Fail/Invalid,
adds column with these values

Writes table to excel file for output.

Analyzes pass/fail/invalid frequency by reader
