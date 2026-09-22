import io
import requests
import pandas as pd


#### METOPS ####

def fetch_smhi_metobs(
    version: str, 
    parameters: list, 
    stations: list, 
    periods: list):
    """
    Queries SMHI MetObs API for multiple parameters, stations, and periods.
    Handles JSON for recent data and complex CSV headers/footers for 'corrected-archive'.
    Returns a unified long-format pandas DataFrame.
    """
    base_url = (
        "https://opendata-download-metobs.smhi.se/api/version/{version}/"
        "parameter/{parameter}/station/{station}/period/{period}/data.{ext}"
    )
    
    all_data = []
    
    for param in parameters:
        for station in stations:
            for period in periods:
                ext = 'csv' if period == 'corrected-archive' else 'json'
                url = base_url.format(
                    version=version,
                    parameter=param,
                    station=station,
                    period=period,
                    ext=ext
                )
                
                try:
                    response = requests.get(url)
                    if response.status_code != 200:
                        print(f"Skipping: HTTP {response.status_code} for URL -> {url}")
                        continue
                        
                    if ext == 'json':
                        data = response.json()
                        param_info = data.get('parameter', {})
                        param_name = param_info.get('name', f'Param_{param}')
                        param_unit = param_info.get('unit', '')
                        
                        station_info = data.get('station', {})
                        station_name = station_info.get('name', f'Station_{station}')
                        
                        values = data.get('value', [])
                        if not values:
                            continue
                            
                        df_temp = pd.DataFrame(values)
                        df_temp['TimeStamp'] = pd.to_datetime(df_temp['date'], unit='ms')
                        df_temp = df_temp.rename(columns={'value': 'Value', 'quality': 'Quality'})
                        
                    else:  # Robust CSV parsing for 'corrected-archive'
                        csv_text = response.text
                        lines = csv_text.splitlines()
                        
                        station_name = f'Station_{station}'
                        param_name = f'Param_{param}'
                        param_unit = ''
                        header_idx = -1
                        
                        # 1. Scan header metadata and find where the data table starts
                        for idx, line in enumerate(lines):
                            parts = [p.strip() for p in line.split(';')]
                            
                            # Look for station name (usually follows 'Stationsnamn')
                            if idx > 0 and 'Stationsnamn' in lines[idx-1]:
                                if len(parts) > 0 and parts[0]:
                                    station_name = parts[0]
                                    
                            # Look for unit / parameter name (usually near 'Enhet' or parameter description rows)
                            if 'celsius' in line.lower() or 'enhet' in line.lower():
                                for p in parts:
                                    if p.lower() in ['celsius', 'm/s', 'mm', '%', 'hpa', 'deg']:
                                        param_unit = p
                                        
                            # Identify the data table header row (contains Datum and Tid)
                            if line.startswith('Datum') or ('Datum;' in line and 'Tid' in line):
                                header_idx = idx
                                # Extract parameter name directly from the data header if available
                                if len(parts) >= 3 and parts[2] not in ['', 'Datum', 'Tid (UTC)']:
                                    param_name = parts[2]
                                break
                        
                        if header_idx == -1:
                            continue
                            
                        # 2. Read the CSV starting from the data header row
                        df_temp = pd.read_csv(
                            io.StringIO(csv_text), 
                            sep=';', 
                            skiprows=header_idx, 
                            header=0,
                            dtype=str,  # to avoid warnings
                            on_bad_lines='skip'
                        )
                        
                        # Standardize columns: Datum, Tid (UTC), [Parameter], Kvalitet
                        if df_temp.shape[1] < 4:
                            continue
                            
                        df_temp = df_temp.iloc[:, :4]
                        df_temp.columns = ['Datum', 'Tid', 'Value', 'Quality']
                        
                        # Drop footer explanation rows where 'Datum' is not a valid date string
                        df_temp = df_temp[df_temp['Datum'].str.match(r'^\d{4}-\d{2}-\d{2}$', na=False)]
                        
                        # Combine Date and Time into a single TimeStamp
                        df_temp['TimeStamp'] = pd.to_datetime(df_temp['Datum'] + ' ' + df_temp['Tid'], errors='coerce')
                        df_temp = df_temp.drop(columns=['Datum', 'Tid'])
                    
                    df_temp['Value'] = pd.to_numeric(df_temp['Value'], errors='coerce')

                    # Map metadata columns uniformly
                    df_temp['ParameterID'] = param
                    df_temp['ParameterName'] = param_name
                    df_temp['Unit'] = param_unit
                    df_temp['StationID'] = station
                    df_temp['StationName'] = station_name
                    df_temp['Period'] = period
                    
                    keep_cols = [
                        'TimeStamp', 'StationID', 'StationName', 
                        'ParameterID', 'ParameterName', 'Unit', 
                        'Value', 'Quality', 'Period'
                    ]
                    df_temp = df_temp[[c for c in keep_cols if c in df_temp.columns]]
                    
                    all_data.append(df_temp)
                    
                except Exception as e:
                    print(f"Error processing Param {param}, Station {station}, Period {period}: {e}")
                    
    if not all_data:
        return pd.DataFrame()
        
    return pd.concat(all_data, ignore_index=True)