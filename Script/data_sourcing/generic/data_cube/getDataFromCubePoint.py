#!/usr/bin/env python
# coding: utf-8

# In[3]:


from datetime import datetime, timedelta
import argparse
import concurrent.futures
import os
import requests
import json
import pandas as pd

"""
    Only for testing purposes
    
    start_month = 12
    start_day = 10
    end_month = 9
    end_day = 5
    number_of_years = 40
    xmin=-90 
    xmax=-83
    ymin=12
    ymax=16
"""






# Generate 1 year for the given interval
url = "https://zarr-query-api-rv7rkv4opa-uc.a.run.app/v1/getdataOnePoint"
def generat_interval_date(start_month:int,
                          start_day:int,
                          end_month:int,
                          end_day:int,
                          number_of_years:int
                         )->[]:
    
    date_ranges = []
    for i in range(0, number_of_years, 1):
        start_year = current_year - i
        if start_month >= end_month:
            end_year = start_year +1
        else :
            end_year = start_year 

        start_date = datetime(start_year, start_month, start_day)
        end_date = datetime(end_year, end_month, end_day)

        date_ranges.append((start_date.strftime('%Y-%m-%d'), 
                          end_date.strftime('%Y-%m-%d')))
    
    
    return date_ranges

def get_response(start_date:str,
                  end_date:str,
                  x:float,
                  y:float,
                  url:str,
                  variable:str,
                  download_path:str
                 ):
    
    headers = {'Accept': 'application/json'}
    
    url_params = {"startDt":start_date,
                  "endDt":end_date,
                  "lon":x,
                  "lat":y,
                  "variableName":variable
    }
    print(url_params)
    response = requests.post(url,
                             headers = headers,
                             data=json.dumps(url_params),
                             stream=True)
# It needs to change to a dataframe
    if response.status_code == 200:
        df = pd.read_json(response.json(),orient='records')     
        filename = f'{variable}_{x}_{y}_{start_date}_{end_date}.csv'
        df.to_csv(download_path+filename,index=False)       
    
    else:
        response.raise_for_status()
        

def process_downloaded_file(file_path):
    print(f"Downloaded file saved to: {file_path}")



if __name__ ==  "__main__":
    parser = argparse.ArgumentParser(description="Data cube downloader")
    parser.add_argument('--startDate', help='Start date of the data cube')
    parser.add_argument('--endDate', help='End date of the data cube')
    parser.add_argument('--x', help='X value')
    parser.add_argument('--y', help='Y value')
    parser.add_argument('--variable', help='Variable to download')
    parser.add_argument('--numberofyears', type=int, default=40, help='Number of years to download')
    parser.add_argument('--lastYear',default=datetime.now().year, help="Where the data have to be downloaded")
    parser.add_argument('--downloadpath',default='/tmp/', help="Where the data have to be downloaded")
    
    args = parser.parse_args()
    current_year = int(args.lastYear)
    start_date = args.startDate.split ('-')
    end_date = args.endDate.split ('-')
    number_of_years = int(args.numberofyears)
    start_month = int(start_date[0])
    start_day = int(start_date[1])
    end_month = int(end_date[0])
    end_day = int(end_date[1])
    x = args.x
    y = args.y

    download_path = args.downloadpath
    
    date_intervals = generat_interval_date(start_month,
        start_day,
        end_month ,
        end_day,
        number_of_years)
    print(x,y)
    variable= args.variable
    with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
        future_to_interval = {executor.submit(get_response,start_date, end_date,x,y,
                                              url,variable,download_path): (start_date, end_date) for start_date, end_date in date_intervals}
            
    for future in concurrent.futures.as_completed(future_to_interval):
        interval = future_to_interval[future]
        try:
            file_path = future.result()
            print(f"Requested from {interval[0]} to {interval[1]}")
            process_downloaded_file(file_path)
        except Exception as exc:
            print(f"Request for interval {interval} generated an exception: {exc}")




