# Scripts to Calculate the Concentration of CO₂, CH₄, and N₂O for Discrete Samples Measured with an Open Loop Method

These scripts calculate the concentrations of CO₂, CH₄, and N₂O based on discrete samples measured using an open loop method (see the [Li-COR note](https://www.licor.cn/uploads/20240112/TGA-Note-Small-Samples_18944.pdf.pdf)).

The scripts (1) integrate the area under the peak produced when injecting a small volume of gas sample into Li-COR portable analyzers (Li-7820 and Li-7810) connected in series and (2) calculate sample concentration in parts per million (ppm) using a one-point calibration factor. Additionally, you can (3) calculate molar GHG water concentration if you have samples originating from the headspace equilibration method. 

These scripts are intended to work with raw-data files downloaded from Li-COR instruments that contain one remark per injection sequence (one unique remark active throughout all replicate injections of same volume of a particular gas sample). You have the option to edit remarks' start,stop and label after acquisition (see steps below).  

###  How to proceed:
1. Download this folder
2. Change the project_root path in all scripts to the local path on your computer where you want to work and store the results. The path should point to a folder that contains a subfolder (called Rawdata) with the raw files. The 'EXAMPLE_PROJECT' can be used as project_root to test scripts behaviour.
3. Place your raw files from the Li-COR 7820 and/or Li-COR 7810 in the Rawdata folder.
4. Run the `1_Map_injections.R` script. This will read and identify remarks recorded during data acquisition.
5. Open the "raw_.\*_map_injection..." files, fill the columns labeled "\*_corrected," and save them as `*.csv` files, replacing "raw_..." with "corrected_..." in the file name.
   You have to specify the upstream-downstream configuration of your instruments by writting "TG10" or "TG20" in the apropriate column. You have the option to copy-paste the auto-filled remark's details (start-end times and label) or to modify them in case of mistakes during acquisition. Corrected labels must be unique for each sample-injectionvolume combination and follow the pattern "samplecode_mlinjected" (underscore is reserved for separating the sample label from the volume of injection).
6. Run the `2_Raw_to_peaks_Licor_per-peakbasecorrection.R` script. This will perform the peak-integration. Check the produced integration plots to identify potential errors in remark specification.
7. If you have a calibration for your instrument (see `*.csv` file in calibration folder) you can run the `3_Peaks_to_ppm_Licor.R` script.
8. Run the `4_Summary_of_samples_Licor.R` script to get summaries of your sample concentrations. 

#### To calculate water concentrations from headspace samples, follow the steps below.

9. Create an auxliary file for your data with script `5_0_Create_auxiliary_template_file`, fill-in the necessary details (see below) and save it as `HeadSpace_auxiliary_template_filled.csv`. 
10. Run the script `5_1_Headspace_to_water_concentration.R`. This script uses the `All_Injections_ppm_ch4_co2_n2o.csv`and `HeadSpace_auxiliary_template_filled.csv` files to calculate water concentrations (in µM and µatm) for your headspace samples. It will return two output files: one containing the calculated concentrations for each injection, and another summarizing the mean values for up to three injections per sample.

### Auxiliary file filling – Requirements for Headspace Calulation
If you want to calculate water concentration from headspace samples, you will need some additional information.
After creating the `HeadSpace_auxiliary_file.csv` file in step 9 (located in the `Auxiliary_files` subfolder), fill in the required details manually and save it as `HeadSpace_auxiliary_template_filled.csv`. You can download an already filled example file [here](EXAMPLE_PROJECT/Auxiliary_files/HeadSpace_auxiliary_template_filled.csv). You must fill the following columns: 

  1. **sampleID**: The same ID you used for injections (it is filled automatically when you create the file in step 9). It usually follows a format like Site-Sample-Replicate.
  
  2. **Sample_type**: Indicates whether the sample corresponds to a headspace sample (HS) or to an atmospheric reference air sample (AtmRef). If the sample should not be used for headspace calculation, enter "ignore" in this field.
  
  3. **LinkID**: A name that links each headspace sample (HS) to its corresponding atmospheric reference air sample (AtmRef). This will depend on your experimental design. Headspaces and atmospheric reference samples will be grouped by the value inputed in the LinkID column. This way you can (1) pair headspace and atmospheric samples one-to-one, (2) use a single atmospheric sample for various headspace samples, (3) use the average value of multiple atmospheric samples for a single headspace sample or (4) use the the average of multple atmospheric samples for various headspace samples. If you do not have atmospheric reference air samples, you can use fixed atmospheric reference values. In that case, this column is not required and you can leave it as `NA`. The unique reference value (for each ghg) should be entered manually in the appropriate nGHG_water_uM function call (line 104 of the script `5_1_Headspace_to_water_concentration.R`) though the `GHG_atm_ppmv` argument by replacing `avg_ppm_reference` with the chosen value (in ppm).
  
  4. **Temp**: Temperature in degrees Celsius at which the headspace equilibration took place.
  
  5. **Vol_air**: Volume of the headspace (air) in milliliters during equilibration.
  
  6. **Vol_water**: Volume of water in milliliters during equilibration.
  
  7. **Pressure**: Atmospheric pressure (in atm) during equilibration.

