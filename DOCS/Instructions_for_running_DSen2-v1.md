# Instructions for running Dsen2 - Super-Resolution of Sentinel-2 Images

[Super-Resolution of Sentinel-2 Images: Learning a Globally Applicable Deep Neural Network](https://arxiv.org/abs/1803.04271)

Download the repository at [https://raw.githubusercontent.com/lanha/DSen2](https://raw.githubusercontent.com/lanha/DSen2)

## Requirements

- tensorflow-gpu (or tensorflow)
- keras
- nupmy
- scikit-image
- argparse
- imageio
- matplotlib (optional)
- GDAL >= 2.2 (optional)

## Installation instructions for Anaconda-3

__NOTE:__ 
Seems to work only (?) with tensorflow==1.14. For v>=2 it breaks... :-( 
Does not work with tensorflow-gpu as errors occur for keras...

1) _Install Anaconda/Python-3.7_

2) _Create a new environment_
```
conda create -n "dsen2" python=3.7

```

3) Activate the newly created environment
```
conda activate dsen2_gpu
```

4) _Run the remaining installation commands in the new environment:_

```
python -m pip install --upgrade pip

pip install msgpack

pip install argparse

conda install scikit-image

conda install imageio

conda install -c conda-forge gdal 

conda install -c conda-forge tensorflow==1.14

conda install -c conda-forge keras 

```

## Usage instructions

- In Windows button/search go to "Anaconda Prompt (Anaconda 3)"

- Using the command prompt, navigate to the DSen2 repository folder

- Inside that directory, go to folder "testing"

- Run the __s2_tiles_supres.py__ python script (which will use a pre-trained deep neural network) 
for a single Sentinel-2 image/scene

Here's an example (replace image input and output paths...):

```python
python s2_tiles_supres.py D:/DATA/S2/L2A/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNF_20190530T132835.SAFE/MTD_MSIL2A.xml C:/MyFiles/temp/S2/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNF_20190530T132835.tif --copy_original_bands
```

Wait like two or three hours!?.... :-/

The write order of the original bands and the super-resolved ones is the following:

- [1] B4 (665 nm)      - Red
- [2] B3 (560 nm)      - Green
- [3] B2 (490 nm)      - Blue
- [4] B8 (842 nm)      - NIR
- [5] SRB5 (705 nm)    - Red Edge 1
- [6] SRB6 (740 nm)    - Red Edge 2
- [7] SRB7 (783 nm)    - Red Edge 3
- [8] SRB8A (865 nm)   - Red Edge 4
- [9] SRB11 (1610 nm)  - SWIR 1
- [10] SRB12 (2190 nm) - SWIR 2


## Additional notes

- Can't find GDAL module issue: to solve this go to the script and import from osgeo