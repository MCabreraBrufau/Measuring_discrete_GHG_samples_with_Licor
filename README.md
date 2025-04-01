# Script to Calculate the Concentration of CO₂, CH₄, and N₂O for Discrete Samples Measured with an Open Loop Method

This script calculates the concentrations of CO₂, CH₄, and N₂O based on discrete samples measured using an open loop method (see the [Li-COR note](https://www.licor.cn/uploads/20240112/TGA-Note-Small-Samples_18944.pdf.pdf)).

The script integrates the area under the peak produced when injecting a small volume of gas sample into two Li-COR devices (Li-7820 and Li-7810) connected in series (the output of one device feeds directly into the input of the other). The method used is based on an open loop system.

The peak detection and area integration script was adapted from [Miguel Cabrera's repository](https://github.com/MCabreraBrufau/Licor_N2O_scripts), originally created for N₂O measurements. The contribution here is simply to modify the script to include calculations for CO₂ and CH₄.

###  How to procedure:
1. Download this folder
2. Place your raw files from the Li-COR 7820 and/or Li-COR 7810 in the Rawdata folder.
3. Run the `1_Map_injections.R` script.
4. Open the "raw_.\*_map_injection..." files, modify the columns labeled "\*_corrected," and save them as `*.csv` files, replacing "raw_.\*_map_injection..." with "corrected_..." in the file name.
   You have to specify in the corrected map injection files the configuration of your instruments by writting "TG10" or "TG20" in the apropriate column. You have the option to modify the remarks recorded during data-aquisition in case of mistakes. Corrected labels must be unique for each sample and follow the pattern "samplecode_mlinjected" (underscore is reserved for separating the volume of injection).
6. Run the `2_Raw_to_peaks_Licor_per-peakbasecorrection.R` script.
7. If you have a calibration for your instrument (see `*.csv` file in calibration folder) you can run the `3_Peaks_to_ppm_Licor.R` script
8. Run the `4_Summary_of_samples_Licor.R` script to get summaries of your sample concentrations. 
