## Context
- This repo contains an example of organizing, summarizing, and plotting data in an RMarkdown document with NOAA LCD weather data
- To start, please open the formatted document named [`example_data_processing.md`](https://github.com/dmgt/example_data_cleaning/blob/master/example_data_processing.md)
- This example is intended to be a simplified example of steps involved to create a figure and table for a scientific report or paper

### Files in this repository
- **cleaned_data** - subfolder containing input file
- **standardized_data** - subfolder of saved csvs after additional data manipulation to append metadata and take hourly averages
- **results** - subfolder for generated figure and plot
- `example_data_processing.Rmd` - RMarkdown notebook for interactive programming, knitted to create a formatted .md file to display on GitHub
- `README.md` -  this file, a general overview of the repository in markdown format.
- `.gitignore` - Optional file, ignore common file types we don't want to accidentally commit to GitHub. Most projects should use this.
- `example_data_cleaning.Rproj`-  Optional, an R-Project file created by RStudio for it's own configuration. 
- **`example_data_processing_files`** - subfolder created by .Rmd --> .md conversion process, holds version of images needed for images to display on GitHub

This file organization is inspired by the following two papers you can check out for more guidance on organizing computaional research projects: 
- [A Quick Guide to Organizing Computational Biology Projects](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424)
- [Good enough practices in scientific computing](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510)

