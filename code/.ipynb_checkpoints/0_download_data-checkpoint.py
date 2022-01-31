# This script downloads ERA5 data and organizes where it is stored. 
# A general function is defined and then called for years 2002-2021. 
# The following variables are called for: 
# - Tmin, Tmax, Tavg 2m temperature
# - Precipitation
# - Ultraviolet radiation
# Anna Boser, Jan 28, 2022

from pyprojroot import here
import cdsapi
import os

def retrieve(year, variable, new_varname):
    
    # tell cdsapi who you are. You need an acconut. See: https://stackoverflow.com/questions/66288678/cds-toolbox-exception-missing-incomplete-configuration
    c = cdsapi.Client() 
    
    filename = 'era5_'+new_varname+'_'+str(year)+'.nc'
    
    # retrieve the data for the year and variable you specified. 
    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type': 'reanalysis',
            'variable': variable,
            'year': str(year),
            'month': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
            ],
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31',
            ],
            'time': [
                '00:00', '01:00', '02:00',
                '03:00', '04:00', '05:00',
                '06:00', '07:00', '08:00',
                '09:00', '10:00', '11:00',
                '12:00', '13:00', '14:00',
                '15:00', '16:00', '17:00',
                '18:00', '19:00', '20:00',
                '21:00', '22:00', '23:00',
            ],
            'format': 'netcdf',
        },
        filename)
    
    print("Data downloaded for ",new_varname," year "+str(year)+". Now moving data.")
    
    # move your data to the appropriate directory in the raw data folder
    source = here("./code/")
    destination = here("./data/raw/"+new_varname+"/")
    os.rename(str(source)+filename, str(destination)+filename)
    
    print("Data moved.")
    
    return None

# define the variables you're interested in 
variables = [
    'total_precipitation', 
    'downward_uv_radiation_at_the_surface', 
    '2m_temperature'
]

new_varnames = [
    'prcp', 
    'uv', 
    'temp'
]
    
    
# make directories. 
# note I could get away without this step -- os would just get me warnings that it had to create the directory if I put something in before creating. 

# make data and raw directories
os.mkdir(here("./data/"))
os.mkdir(here("./data/raw"))

# make the directories for the variables
for v in new_varnames:
    os.mkdir(here("./data/raw/"+v))

    
# download data 2002-2021 for each variable
for i in range(len(variables)):
    for y in range(2002, 2022):
        retrieve(y, variable[i], new_varname[i])