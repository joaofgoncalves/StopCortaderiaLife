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
seems to work only (?) with tensorflow==1.14. For v>=2 it breaks... :-( Not sure if it works with tensorflow-gpu

1) _Install Anaconda/Python-3.7_

2) _Create a new environment_
```
conda create -name "dsen2"
```

3) _Run the remaining installation commands in the new environment:_

```
pip install argparse

conda install scikit-image

conda install imageio

conda install -c conda-forge gdal 

conda install -c conda-forge tensorflow==1.14

conda install -c conda-forge keras 
```

## Usage instructions

- In Windows button/search go to "Anaconda Prompt (Anaconda 3)"

- Navigate to the DSen2 repository folder

- Inside that directory, go to folder "testing"

- Run the __s2_tiles_supres.py__ python script (which will use a pre-trained deep neural network) for a single sentinel image/scene

Here's an example (replace image input and output paths...):

```python
python s2_tiles_supres.py D:/DATA/S2/L2A/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNF_20190530T132835.SAFE/MTD_MSIL2A.xml C:/MyFiles/temp/S2/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNF_20190530T132835.tif --copy_original_bands
```

Wait like two or three hours!?.... :-/