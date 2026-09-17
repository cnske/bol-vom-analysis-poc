#%% 
""" 
API request for SMHIs metobs data. 

A documentation of the API can be found here: 
https://opendata.smhi.se/metobs/introduction

Known issues: 
- METOPS may offer the paramter [data], but this is not fully working
- no [data] parameter for HYDROPS
"""

# --- Imports ---
import os
import requests
import pandas as pd

from io import StringIO
from functools import reduce

#%%
### METOPS ### 
# --- Perform API call ---
def SMHI_Metops(api_parameter:dict):
    """
    API request for meteorological observations of SMHIs data
    ATTENTION: The data gets resampled to daily means.
    
    Args: 
        api_paramter: a dictionary containing the parameters to request
    Return: 
        A pandas DataFrame containing the requested data.
    """

    os.system('clear')
    print(f"Request SMHI Open Data Meteorological Observations.")

    base_url_meteo = "https://opendata-download-metobs.smhi.se/api"

    dfs = []
    for par in api_parameter['parameter']:
        requst_url = (
            f"{base_url_meteo}/version/{api_parameter['version']}"
            f"/parameter/{par}"
            f"/station/{api_parameter['station']}"
            f"/period/{api_parameter['period']}"
            f"/data.{api_parameter['data']}"    # For older data on .csv available 
        )

        response = requests.get(url=requst_url)

        if api_parameter['data']=='csv':
            csv_data = StringIO(response.text)

            tmp_df = pd.read_csv(
                csv_data, 
                sep=';',
                skiprows=9,
                usecols=[0, 1, 2], 
            )

            tmp_df['DateTime'] = pd.to_datetime(tmp_df['Datum'] + ' ' + tmp_df['Tid (UTC)'])
            tmp_df.drop(['Datum', 'Tid (UTC)'], axis=1, inplace=True)

            dfs.append(tmp_df)
        
        if api_parameter['data']=='json':
            data_json = response.json()
 
            # Extract metadata
            station_name = data_json['station']['name']
            station_id = data_json['station']['key']
            parameter_name = data_json['parameter']['name']
            parameter_unit = data_json['parameter']['unit']

            tmp_df = pd.DataFrame(data_json['value'])

            # Convert epoch to Timestamp
            tmp_df['date_real'] = pd.to_datetime(tmp_df['date'], unit='ms')

            tmp_df.rename(columns={
                'date' : 'DateTime',
                'value' : f"{parameter_name}" #[{parameter_unit}]", 
            }, 
            inplace=True
            )

            tmp_df['DateTime'] = tmp_df['date_real']
            tmp_df['DateTime'] = pd.to_datetime(tmp_df['DateTime'])
            tmp_df = tmp_df.drop(['date_real', 'quality'], axis=1)

            #tmp_df['StationName'] = station_name
            #tmp_df['StationID'] = station_id

            dfs.append(tmp_df)

    df_metops = reduce(lambda left, right: pd.merge(left, right, on=['DateTime'], how='outer'), dfs)

    # Convert to numeric
    cols=[i for i in df_metops.columns if i not in ["DateTime"]]
    for col in cols:
        df_metops[col]=pd.to_numeric(df_metops[col])

    # Non-Generic resamplig, needs to be adjusted
    df_metops = df_metops.resample(rule='D', on='DateTime').agg({
        'Nederbördsmängd' : 'sum', 
        'Lufttemperatur' : 'mean',
        'Vindhastighet' : 'mean',
        'Daggpunktstemperatur' : 'mean',
        'Relativ Luftfuktighet' : 'mean'
    } 
    ).reset_index()

    del dfs

    return df_metops
# %%
### HYDROPS ###
# --- Perform API call ---
def SMHI_Hydroops(api_parameter:dict):
    """
    API request for hydroligical observations of SMHIs data
    ATTENTION: The data gets resampled to daily values.
    
    Args: 
        api_paramter: a dictionary containing the parameters to request
    Return: 
        A pandas DataFrame containing the requested data.
    """

    os.system('clear')
    print(f"Request SMHI Open Data Hydrological Observations.")

    base_url_hydro = "https://opendata-download-hydroobs.smhi.se/api"

    dfs = []
    for station in api_parameter['station']:    
        for par in api_parameter['parameter']:
            requst_url = (
                f"{base_url_hydro}/version/{api_parameter['version']}"
                f"/parameter/{par}"
                f"/station/{station}"
                f"/period/{api_parameter['period']}"
                f"/data.json"
            )
            response = requests.get(url=requst_url)

            if response.status_code == 200:
                data_json = response.json()

                # Extract metadata
                station_name = data_json['station']['name']
                parameter_name = data_json['parameter']['name']
                parameter_unit = data_json['parameter']['unit']

                tmp_df = pd.DataFrame(data_json['value'])

                # Convert epoch to Timestamp
                tmp_df['date_real'] = pd.to_datetime(tmp_df['date'], unit='ms')

                tmp_df.rename(columns={
                    'date' : 'DateTime',
                    'value' : f"{parameter_name} [{parameter_unit}]", 
                }, 
                inplace=True
                )

                tmp_df['DateTime'] = tmp_df['date_real']
                tmp_df['DateTime'] = pd.to_datetime(tmp_df['DateTime'])
                tmp_df = tmp_df.drop(['date_real', 'quality'], axis=1)

                # Resample to daily means
                tmp_df = tmp_df.resample(rule='D', on='DateTime').mean().reset_index()

                tmp_df['StationName'] = station_name

                dfs.append(tmp_df)

            else:
                continue

    #df_hydrops = reduce(lambda left, right: pd.merge(left, right, on=['DateTime'], how='outer'), dfs)

    df_hydrops = pd.concat(dfs, ignore_index=True)
    df_hydrops = df_hydrops.groupby(['DateTime', 'StationName']).first().reset_index()
    
    df_hydrops['DateTime'] = pd.to_datetime(df_hydrops['DateTime'])
    
    del dfs

    return df_hydrops
# %%

### S-Hype Modell data ###
def SHype_Data(links:list):
    """
    Retrieve the daily values of discharge "Dygnsvärden" from SMHIs 
    S-Hype model of the given link and store them inside a single pandas
    DataFrame. Each river is identified with its ID. 

    Args: 
        links: a list containing the links to process
    Return: 
        A pandas DataFrame.
    """
    os.system('clear')
    print(f"Request SMHI S-Hype data.")

    dfs = []
    for link in links:
        name = pd.read_excel(link,
                             sheet_name='Områdesinformation',
                             skiprows=8,
                             usecols=[1],
                             skipfooter=64
                )
        tmp_df = pd.read_excel(link,
                               sheet_name='Dygnsvärden', 
                               skiprows=6, 
                               skipfooter=1, 
                               usecols=[0, 1], 
                               header = 0, 
                               names = ['Datum',f"Q {name.columns[0]}"]
                    )

        dfs.append(tmp_df)

        del tmp_df
        del name 

    df_SHype = reduce(lambda left, right: pd.merge(left, right, on=['Datum'], how='outer'), dfs)

    df_SHype['DateTime'] = pd.to_datetime(df_SHype['Datum'])
    df_SHype.drop('Datum', axis=1, inplace=True)

    df_SHype.dropna(how='all', axis=0, inplace=True)

    return df_SHype