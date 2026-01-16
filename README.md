# Script to Calculate the Concentration of CO₂, CH₄, and N₂O for Discrete Samples Measured with an Open Loop Method

This script calculates the concentrations of CO₂, CH₄, and N₂O based on discrete samples measured using an open loop method (see the [Li-COR note](https://www.licor.cn/uploads/20240112/TGA-Note-Small-Samples_18944.pdf.pdf)).

The script integrates the area under the peak produced when injecting a small volume of gas sample into two Li-COR devices (Li-7820 and Li-7810) connected in series (the output of one device feeds directly into the input of the other). The method used is based on an open loop system.

The peak detection and area integration script was adapted from [Miguel Cabrera's repository](https://github.com/MCabreraBrufau/Licor_N2O_scripts), originally created for N₂O measurements. The contribution here is simply to modify the script to include calculations for CO₂ and CH₄.

###  How to procedure:
1. Download this folder
2. Change the project_root path in all scripts to the local path on your computer where you want to work and store the results. The path should point to a folder that contains a subfolder (called Rawdata) with the raw files. The 'EXAMPLE_PROJECT' can be used as project_root to test scripts behaviour.
3. Place your raw files from the Li-COR 7820 and/or Li-COR 7810 in the Rawdata folder.
4. Run the `1_Map_injections.R` script.
5. Open the "raw_.\*_map_injection..." files, modify the columns labeled "\*_corrected," and save them as `*.csv` files, replacing "raw_.\*_map_injection..." with "corrected_..." in the file name.
   You have to specify in the corrected map injection files the configuration of your instruments by writting "TG10" or "TG20" in the apropriate column. You have the option to modify the remarks recorded during data-aquisition in case of mistakes. Corrected labels must be unique for each sample and follow the pattern "samplecode_mlinjected" (underscore is reserved for separating the volume of injection).
6. Run the `2_Raw_to_peaks_Licor_per-peakbasecorrection.R` script.
7. If you have a calibration for your instrument (see `*.csv` file in calibration folder) you can run the `3_Peaks_to_ppm_Licor.R` script
8. Run the `4_Summary_of_samples_Licor.R` script to get summaries of your sample concentrations. 

#### To calculate water concentrations from headspace samples, follow the steps below.

9. Create an auxliary file. You can download an example auxiliary file [here](EXAMPLE_PROJECT/Auxiliary_files/HeadSpace_auxiliary_template_filled.csv). You can create this file yourself or run the script `5_0_Create_auxiliary_template_file` to generate a template for your samples and enter the information manually. 
10. Run the script `5_1_Headspace_to_water_concentration.R`. This script uses the files `All_Injections_ppm_ch4_co2_n2o.csv`and `HeadSpace_auxiliary_template_filled.csv` to calculate water concentrations (in µM and µatm) for your headspace samples. It will return two output files: one containing the calculated concentrations for each injection, and another summarizing the mean values for up to three injections per sample.

### Additional Information – Requirements for Headspace Calulation
If you need to calculate water concentration from headspace samples, you will need some additional information.

You will need an auxiliary file called `HeadSpace_auxiliary_file.csv`, located in a folder named `Auxiliary_files` within your working directory, containing the following columns:

  1. **sample**: The same ID you use for injections. It usually follows a format like Site-Sample-Replicate.
  
  2. **Sample_type**: Indicates whether the sample corresponds to a headspace sample (HS) or to an atmospheric reference air sample (AtmRef). If the sample should not be included in the analysis, enter "ignore" in this field.
  
  3. **LinkID**: A name that links each headspace sample (HS) to its corresponding atmospheric reference air sample (AtmRef). This depends on your experimental design. In some cases, it may match the Sample name (when you have one or more atmosphere references per sample), or the Site name (when you have one or more atmosphere references per site). If you do not have atmospheric reference air samples, you can use fixed atmospheric reference values from a nearby monitoring station or from NOAA (add reference). In that case, this column is not required and you can leave it as `NA`. The unique reference value should be entered manually in lines 85–87 of the script (`5_1_Headspace_to_water_concentration.R`) though the `GHG_atm_ppmv` argument by replacing `avg_ppm_reference` with the chosen value.
  
  4. **Temp**: Temperature in degrees Celsius at which the headspace equilibration took place.
  
  5. **Vol_air**: Volume of the headspace (air) in milliliters during equilibration.
  
  6. **Vol_water**: Volume of water in milliliters during equilibration.
  
  7. **Pressure**: Atmospheric pressure (in atm) during equilibration.

You can download an example auxiliary file [here.](EXAMPLE_PROJECT/Auxiliary_files/HeadSpace_auxiliary_template_filled.csv)
