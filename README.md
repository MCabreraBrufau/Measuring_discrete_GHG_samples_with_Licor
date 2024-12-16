# Script to Calculate the Concentration of CO₂, CH₄, and N₂O for Discrete Samples Measured with an Open Loop Method

This script calculates the concentrations of CO₂, CH₄, and N₂O based on discrete samples measured using an open loop method (see the [Li-COR note](https://www.licor.cn/uploads/20240112/TGA-Note-Small-Samples_18944.pdf.pdf)).

The script integrates the area under the peak produced when injecting a small volume of gas sample into two Li-COR devices (Li-7820 and Li-7810) connected in series (the output of one device feeds directly into the input of the other). The method used is based on an open loop system.

The peak detection and area integration script was adapted from [Miguel Cabrera's repository](https://github.com/MCabreraBrufau/Licor_N2O_scripts), originally created for N₂O measurements. The contribution here is simply to modify the script to include calculations for CO₂ and CH₄.

### Requirements:
1. Raw files from the Li-COR 7820 and/or Li-COR 7810.
2. Run the `Map_injections.R` script.
3. Open the "raw_.*_map_injection..." files, modify the columns labeled "*_corrected," and save them as `*.csv` files with the corrected data instead of the raw data.
4. Run the `Raw_to_peaks_Licor.R` script.
