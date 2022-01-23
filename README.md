# climate-aggregate
This repository develops a generalizable pipeline to aggregate high spatial and temporal resolution climate data to arbitrary polygons and select temporal periods. 

## Initial use-case
We are developing this codebase with a few initial specificaitons, listed below. We ultimately aim to generalize the approach to arbitrary gridded climate datasets, nonlinear transformations, and temporal aggregatins. Initial specs:

- ERA 5 climate data
- Tmin, Tmax, Tavg 2m temperature
- Precipitation
- Ultraviolet radiation
- Polynomial, restricted cubic spline, and binned transformations
- Daily, monthly, or annual output

