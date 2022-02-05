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

def retrieve(year, variable, new_varname, month):
    
    # tell cdsapi who you are. You need an acconut. See: https://stackoverflow.com/questions/66288678/cds-toolbox-exception-missing-incomplete-configuration
    c = cdsapi.Client() 
    
    filename = 'era5_{new_varname}_{year}_{month}.nc'.format(
        new_varname=new_varname,
        year=year, 
        month=month
    )
    relative_outpath = '../data/raw/{new_varname}/'.format(new_varname=new_varname)
    
    # retrieve the data for the year and variable you specified. 
    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type': 'reanalysis',
            'variable': variable,
            'year': str(year),
            'month': month,
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
        relative_outpath+filename)
    
    print("Data downloaded for ",new_varname," month-year "+month+"-"+str(year)+".")
    
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


# make the directories for the variables
for v in new_varnames:
    if not os.path.exists(here("./data/raw/"+v)):
        os.makedirs(here("./data/raw/"+v))


months = [
    '01', '02', '03',
    '04', '05', '06',
    '07', '08', '09',
    '10', '11', '12',
]
    
# download data 2002-2021 for each variable
for i in range(len(variables)):
    for y in range(2002, 2022):
        for month in months:
            retrieve(y, variables[i], new_varnames[i], month)