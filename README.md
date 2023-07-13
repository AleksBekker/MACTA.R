# MACTA.R

R package for running multiple automated cell type annotation packages

## Implemented tools:
- SingleR - Uploaded
- SCINA - Uploaded
- Garnett - Uploaded
- UCell - Uploaded
- scPred - Uploaded
- scCATCH - Uploaded
- Symphony - Uploaded
- scMAP - Uploaded
- scMRMA - Uploaded
- scSorter
- scMAGIC

## Testing

Run the test_all script, it creates the needed objects. You'll need to have the packages first though. I'm working on a conda env file to get those into R for now

## Current task list:

1. Implement validators
2. ~~Re-factor code into a more standardized and extensible format~~
3. ~~Standardize environment for all R packages present~~
4. ~~Share small testing environment with objects. Look into how this can be put into a larger framework rather than rudimentary R scripts.~~
5. Extend methodology and add simulated datasets, as well as several real ones.
6. Benchmarking
