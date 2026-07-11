# PPE_performance

1. The .Rproj file must be in the same folder that contains "current" folder.
2. Specify the correct parameters in main_batch.R and run. Outputs (tables/data) are generated in respective folders. If you want to rerun using different parameters, its best to delete all files in the tables and data folders because some might not be overwritten otherwise.
3. To extract weights, run aggregate_weight_analysis.R
4. Generate a report with final_report.qmd in "report." Depending on the size of the files being aggregated, it may be possible just to render the full file. If this fails, then run each code block sequentially and view the plots this way.
5. The weighting and cohort diagnostic plots are currently commented out because the file sourcing might not be working properly at this time.
