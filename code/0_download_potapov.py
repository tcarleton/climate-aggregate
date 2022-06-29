# This script downloads the potapov cropland maps available at https://glad.umd.edu/dataset/croplands

# Anna Boser Feb 8, 2022

import requests
import os

root = '/home/tcarleton/Climate/'
data_folder = root + 'data'
# data_folder = '../data' # if running locally

def download_potapov(year, quadrant):
    
    url = 'https://glad.geog.umd.edu/Potapov/Global_Crop/Data/Global_cropland_{quadrant}_{year}.tif'.format(
        year=year, 
        quadrant = quadrant)

    filename = 'cropland_{quadrant}_{year}.tif'.format(
        quadrant=quadrant,
        year=year
    )

    outpath = '{data_folder}/raw/weights/cropland/'.format(
        data_folder=data_folder)
    if not os.path.exists(outpath):
        os.makedirs(outpath)

    r = requests.get(url, allow_redirects=True)
    open(outpath+filename, 'wb').write(r.content)
    
    print("Data downloaded for ", quadrant," year "+str(year)+".")
    
    return None
    
for year in range(2003, 2019+1,4): # all years available
    for quadrant in ["NE", "NW", "SE", "SW"]: # all four quadrants of the globe
        download_potapov(year, quadrant)
        
