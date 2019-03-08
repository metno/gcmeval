#!/usr/bin/env python
import cdsapi, sys, getopt, os
from datetime import date
from dateutil.relativedelta import relativedelta

#opts, args = getopt.getopt(sys.argv[1:], "f:l:v:t:r")
#for opt, param in opts:
#  if opt == "-f":
#    firstYear = param
#  elif opt == "-l":
#    lastYear = param
#  elif opt == "-v":
#    variable = param
#  elif opt == "-t":
#    type = param
#  elif opt == "-r":
#    stream = param
#  elif opt == "-o":
#    outfile = param
#  else:
#    assert False, "unhandled option"

def genDates(start,end):
  time = date(int(start), 1, 1)
  string = [time.strftime("%Y-%m-%d")]
  while True:
    time = time + relativedelta(months=+1)
    if((time.year > int(end)) or (time > date.today() + relativedelta(months=-2)) ):
      break
    string.append(time.strftime("%Y-%m-%d"))
  return string
      
def era5_request(requestDates, decade, target):
  c = cdsapi.Client()
  c.retrieve('reanalysis-era5-complete', {
    'class'  :'ea',
    'expver' : '1',
    'levtype': 'sfc',
    'stream' : stream,
    'param'  : variable,
    'type'   : type,
    'date'   : requestDates,
    'decade' : decade
  }, target)

def retrieve_era5(start, end):
  years = range(start, end+1)
  decades_years = [divmod(i, 10)[0]*10 for i in years]
  decades_list = list(set(decades_years))
  decades_list.sort()
  for d in decades_list:
    is_decade = [dy==d for dy in decades_years]
    index_decade = [i for i, x in enumerate(is_decade) if x]
    years_d = [years[i] for i in index_decade]
    requestDates = genDates(min(years_d), max(years_d))
    target = 'era5_%s_%s_%s_%d.grib'% (stream,variable,type,d)
    if(not os.path.isfile(target)):
      era5_request(requestDates, d, target)
  files = [f for f in os.listdir('.') if \
           re.match(r'era5_%s_%s_%s_[0-9]{4}.grib'% (stream,variable,type), f)]
  syscmd = 'cat '
  for f in files:
    syscmd = syscmd+f+' '
  syscmd = syscmd+outfile
  os.system(syscmd)

# Retrieve data:
#retrieve_era5(firstYear,lastYear)

