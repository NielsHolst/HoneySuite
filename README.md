# HoneySuite
## Start here
1. Click the **green button**. Download and unzip.
2. Open the file *src/lib/configure.R*. Run just the **library** lines to check that you've got all the needed packages installed. Install any missing.
3. Open the file *src/process_input_files.R*. Edit **setwd(..)** to reflect the path to your *HoneySuite/src* folder.
4. Run *src/process_input_files.R*. After a couple minutes it will have finished.
5. Study the files in *HoneySuite/output*:
    1. *xxx-W.Rdata* contains the original data from the file *input/xxx.txt* but converted to solar time and rinsed for outliers and weight noise. 
    2. *xxx-WD.Rdata* contains daily summary parameters.
    3. *xxx-noise_dropped* contains observations that were dropped due to weight noise.

## Plotting
1. Open *src/plot-az.R* and edit its **setwd(..)** as above.
2. Run it. It produces some sample outputs:
    1. A plot on the screen.
    2. A pdf file with various plots found in *output/plot-az.pdf*.  

## Your own code
1. Begin any script with **two lines** identical to those found in *src/plot-az.R*.
2. As exemplified in the same script, **get the metadata** for the input files and pick the data sets you want to work on.
3. Write your own code.

## Input data format
1. All input data are **tab-separated**, column-oriented text files. 
    1. Set up data in a spreadsheet
    2. Mark the block with your data. Copy to clipboard.
    3. In Notepad++, paste and then save. This procedure will autimatically result in tab stops between columns.
2. All dates must use **4-digit years**. These formats ought to work:
    1. m/d/y
    2. d/m/y 
    3. y/m/d
3. All dates with time should be written in one of these formats (seconds are optional): 
    1. m/d/y H:M:S
    2. d/m/y H:M:S
    3. y/m/d H:M:S
    
## Adding new data sets
1. Add a row to *input/meta-data.txt* with the information and your data set.
2. Create a text file with hive data (see formats above) and save it as */input/xxx.txt*.
  1. First column must contain a **timestamp**.
  2. The following columns must all contain **weight readings**, one column for each one hive. The column headers identify the hives within this data set.
3. Create a text file with dates or date*hive combinations that should be **excluded**. Save it as */input/xxx-exclude.txt*. The file is obligatory. If you need no exclusions provide a file with the column headings only.
    1. Study one of the existing exclude files and copy its layout (e.g., load it into a spreadsheet).
    2. The **Hives** column can contain either an asterisk (meaning all hives), or one or move hives names (separated by commas).
4. Optionally, provide a **treatments file** as */input/xxx-treatments.txt*. 
    1. The first column must be called *Hive* and contain the hive ids in the data set.
    2. There can be one or more columns after that identifying the treatment. The plot functions have currently been set up to expect a column called **Treatment**.
  3. See existing treatment files for examples.
  
