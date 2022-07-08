## Datasets

### CoeSTORET2002_2011.xlsx
This data is from 2002-2011. It contains 2717 observations as well as a table outlining sampling methods.

#### Sampling Methods
| Measurement | Method | Det. Limit |
|-------------|--------|------------|
| TSS | Standard Methods, 20th edition, Method 2540 D (gravimetric) | 0.1 mg/L |
| DRP | 2009 and earlier-Hach method 8048 (equiv. to EPA365.2) | 0.02 mg PO4/L |
|  | 2010 and later-Lachat FIA method 10-115-01-1-P | 0.005 mg PO4/L |
| Cl | Standard Methods, 20th ed., Method 4110B (ion chromatography) | 0.5 mg/L Cl |
| NO3-N | Standard Methods, 20th ed., Method 4110B (ion chromatography) | 0.2 mg/L NO3-N |
| SO4 | Standard Methods, 20th ed., Method 4110B (ion chromatography) | 0.5 mg/L SO4 |
| E. coli | IDEXX Colilert/QuantiTray200 MPN (began in 2007) | 1 CFU/100 mL |


### 2012-16data.xlsx
This data is from 2012-2016 and contains 862 observations.


### 2017_21Data.xlsx
This data is from 2017-2021 and contains 457 observations.


### newdata.xlsx
This dataset contains 2709 observations with many missing values from previous datasets.


### IndianCreek2012-2021.xlsx
This dataset contains 817 observations with up-to-date Indian Creek Watershed data.


### combined_data_clean4.csv
This dataset contains 3987 observations and 15 variables. The watershed variable was added, the date column was fixed, appropriate values were set to be numeric, and some site names were changed. One row which had all NAs except for the site variable was removed. This is the final dataset with all data added.

#### Variables and Units
| Variable | Meaning | Units |
|----------|---------|-------|
| Watershed | Which watershed the site is in | NA |
| Site | Where sample was taken | N/A |
| Date | Date sample was taken | N/A |
| Time | Time sample was taken | N/A |
| DO | Dissolved Oxygen | mg O2/L |
| Temp | Temperature of Water | Degrees Celsius |
| pH | Acidity | Logarithmic Scale |
| Cond | Conductivity | S/cm |
| Turbidity | Clarity of Water | NTU |
| TSS | Total Suspended Solids | mg/L |
| DRP | Dissolved Reactive Phosphorus | mg PO4/L |
| Cl | Chloride | mg Cl/L |
| NO3-N | Nitrate Nitrogen | mg NO3-N/L |
| SO4/L | Sulfate | mg SO4/L |
| E coli | Colony Forming Units of E. coli | CFU/100 mL |


### CR_airport_rainfall.csv
This dataset includes dates and rainfall estimates for every day from 2002 to 2021.


### watershed_geo
This is a folder containing all geographical data used to create the interactive maps.
