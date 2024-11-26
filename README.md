# broman-etal_2025_wrr

**Title:** How Hydropower Operations Mitigate Flow Forecast Uncertainties to Maintain Grid Services in the Western U.S.

**Authors:**
Daniel Broman*, Nathalie Voisin, Nathalie Voisin, Scott Steinschneider, Jordan Kern, Sungwook Wi, and Henry Ssembatya

\* corresponding author: daniel.broman@pnnl.gov

![Annual Hydropower Revenue Differences When Scheduling With Different Inflow Forecast](https://github.com/HydroWIRES-PNNL/broman-etal_2025_wrr/blob/main/figures/figure7_annualrevdist.png)

## Abstract

Hydropower facilities represent a key electricity generating resource in the U.S. Western Interconnection. These facilities may rely upon forecasts of inflow when scheduling releases to generate electricity. In order to examine how inflow forecasts with differing levels of skill impact hydropower scheduling, and resulting hydropower revenue, we developed a novel generic dynamic-programing hydropower scheduler, Forecast-Informed Scheduler for Hydropower (FIScH) that captures non-powered water management objectives and constraints and allows for varying electricity prices. This scheduler was applied at 242 hydropower facilities, representing 86% of the conventional hydropower nameplate capacity in the Western US interconnect. Hydropower revenues were examined for schedules developed using three different inflow forecasts with different levels of skill – perfect, synthetic, and persistence – over a 20-year period from 2000 to 2019. In aggregate hydropower revenue decreases 0.08% when using a synthetic forecast schedule, and 0.11% when using a persistence forecast schedule as compared to revenue using a perfect forecast schedule. Differing forecast skill resulted in larger revenue differences on the west coast, particularly in California and Oregon, and smaller differences across the interior western U.S. These results provide an understanding of how differing inflow forecast quality impacts hydropower revenue at the scale of the electricity grid, and showcases a novel method for conducting large-scale hydropower studies.

## Journal reference

[to be filled in upon publication]

## Data references
### Input data
|              Dataset              |                                   DOI                                    |
|:---------------------------------:|:------------------------------------------------------------------------:|
|  mosartwmpy managed river flow simulations  | https://doi.org/10.5281/zenodo.13923721                        |


### Output data
The experiment files (resulting from the analysis scripts listed below) are are available at the following DOI.

|       Dataset       |                  DOI                   |
|:-------------------:|:-----------------------------------------------------------------------------:|
| FIScH Simulations |  XXX |


## Contributing modeling software
|  Model   | Version |         Repository Link          | DOI |
|:--------:|:-------:|:--------------------------------:|:---:|
| mosartwmpy |  v0.4.4  | https://github.com/IMMM-SFA/mosartwmpy | NA |
| FIScH |  v0.4.0  | https://github.com/HydroWIRES-PNNL/fisch | NA |
| GO-WEST |  XX   | https://github.com/romulus97/IM3-GO-WEST | XX |
| hydrofixr | NA | https://github.com/pnnl/hydrofixr/tree/master | NA|
| starfit | v0.1.0 | https://github.com/IMMM-SFA/starfit | NA |


## Reproduce my analysis
Clone this repository to get access to the scripts used to conduct the experiment. You'll also need 
to download the input files into the `data/input_data` directory. Each script that requires input data will specify where to download the data. 
Once you have the input datasets downloaded you can use the following 
scripts to rerun the experiment and produce the experiment files. 

To complete the analysis end to end, run the following scripts:

| Script Type      |               Script Name               |                                                                       Description                                                                       |
|:-----------------|:---------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------:|
| Data preparation | 1_create_inflow_timeseries.R | Reads in mosartwmpy simulated streanflow and creates an inflow timeseries for each hydropower facility used in this experiment. These inflows are used to represent perfect inflow forecasts |
| Data preparation | 2_run_starfit-hydrofixr.R | Uses ISTARF data-driven reservoir operating rules and hydrofixr hydropower generation methods to derive weekly reservoir storage targets and weekly power factors |
| Data preparation | 3_map_lmps_to_facilities.R  | Maps GO-WEST locational marginal prices (LMPs) provided by bus (node) to each hydropower facility |
| Data analysis | 4a_run_fisch_ror.R  | Runs hydropower scheduler for run-of-river hydropower facilities; code runs one facility at a time for all three forecast types |
| Data analysis | 4b_run_fisch_storage.R  | Runs hydropower scheduler for storage hydropower facilities; code runs one facility at a time for all three forecast types |


## Reproduce my figures
Use the following scripts to reproduce the figures in this publication. Data analysis is performed in these scripts using data from FIScH.

| Figure Number |                Script Name                 |                                  Description                                   | 
|:--------------:|:------------------------------------------:|:------------------------------------------------------------------------------:|
| 1 | NA | Overview of modeling experiment design |
| 2 | NA | Flow chart showing data flow through each model |
| 3 | f3_plot_hydropower_facility_map.R | Map of hydropower facilities used in experiment by nameplate capacity and operation type |
| 4 | NA | Topology of full 10,000 node GO-WEST model and reduced 100 node model provided by NC State University |
| 5-7 | f4-7_plot_revenue_change.R | Revenue difference for revenue produced by hydropower schedules using different inflow forecasts |
