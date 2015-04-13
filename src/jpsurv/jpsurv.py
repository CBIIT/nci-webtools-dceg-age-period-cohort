#!flask/bin/python
from flask import Flask, render_template, Response, abort, request, make_response, url_for, jsonify, redirect
from functools import wraps
from flask import current_app

import rpy2.robjects as robjects
#from rpy2.robjects import r

import cgi
import shutil
import os
from xml.sax.saxutils import escape, unescape
from socket import gethostname
import json
import pandas as pd
import numpy as np
from pandas import DataFrame
import urllib
from werkzeug import secure_filename
from random import randint
import subprocess


app = Flask(__name__, static_folder='', static_url_path='/')
#app.debug = True
UPLOAD_DIR = os.path.join(os.getcwd(), 'tmp')

#TESTING AREA
HEADER = '\033[95m'
OKBLUE = '\033[94m'
OKGREEN = '\033[92m'
WARNING = '\033[93m'
FAIL = '\033[91m'
BOLD = '\033[1m'
UNDERLINE = '\033[4m'
ENDC = '\033[0m'

print BOLD+UPLOAD_DIR+ENDC

def index():
    return render_template('index.html')

def jsonp(func):
    """Wraps JSONified output for JSONP requests."""
    @wraps(func)
    def decorated_function(*args, **kwargs):
        callback = request.args.get('callback', False)
        if callback:
            data = str(func(*args, **kwargs).data)
            content = str(callback) + '(' + data + ')'
            #mimetype = 'application/javascript'
            mimetype = 'application/json'
            return current_app.response_class(content, mimetype=mimetype)
        else:
            return func(*args, **kwargs)
    return decorated_function

@app.route('/jpsurvRest/hello', methods = ['GET'])
def hello():
    # python LDpair.py rs2720460 rs11733615 EUR 38
    mimetype = 'application/json'

    print
    print 'Execute jpsurvRest/hello'
    print 'Gathering Variables from url'
    print
    #out_json = json.dumps(["foo", {"bar":["baz", null, 1.0, 2]}])
    #my_json = json.dumps([{"Age groups": ["0-49","50-65s","65+"], "Breast stage": ["Localized","Regional","Distant"],"Test group": ["val1","ValTwo","AnotherValue"]}])
    #data =[{'Age groups': ['0-49','50-65s','65+'], 'Breast stage': ['Localized','Regional','Distant'],'Test group': ['val1','ValTwo','AnotherValue']}]
    #data2 = [{"SystemInfo":{"ItemNameInDic":["Output filename","Matrix filename","Database name"],"ItemValueInDic":["h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.txt","h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.ssm","Incidence - SEER 18 Regs Research Data + Hurricane Katrina Impacted Louisiana Cases, Nov 2013 Sub (1973-2011 varying) - Linked To County Attributes - Total U.S., 1969-2012 Counties"]},"SessionOptionInfo":{"ItemNameInDic":["Type","Rate filename","Statistic","SurvivalMethod","SurvivalBeginMonth","SurvivalBeginYear","SurvivalEndMonth","SurvivalEndYear","SurvivalVitalStatus","StudyCutoffDate","LostToFollowupDate","NumberOfIntervals","MonthsPerInterval","RatesDisplayedAs"],"ItemValueInDic":["Survival","U.S. 1970-2009 by individual year (White, Black, Other (AI/API), Ages 0-99, All races for Other Unspec 1991+ and Unknown)","Relative Survival","Actuarial","Month of diagnosis recode","Year of diagnosis","Month of follow-up recode","Year of follow-up recode","Vital status recode (study cutoff used)","12/2011","12/2011","36","12","Percents"]},"ExportOptionInfo":{"ItemNameInDic":["GZipped","Variable format","File format","Field delimiter","Missing character","Fields with delimiter in quotes","Remove thousands separators","Flags included","Variable names included","Column Variables as Stats"],"ItemValueInDic":["false","numeric","DOS/Windows","tab","period","false","true","false","false","false"]},"VarAllInfo":{"ItemNameInDic":["Var1Name","Var2Name","Var2Base","Var3Name","Var3Base","Var4Name","Var4Base","Var5Name","Var6Name","Var7Name","Var8Name","Var9Name","Var10Name","Var11Name","Var12Name","Var13Name","Var14Name","Var15Name","Var16Name","Var17Name","Var18Name"],"ItemValueInDic":["Page type","Age groups","Age recode with <1 year olds","Breast stage","SEER historic stage A","Year of diagnosis: Year of diagnosis 1975+","Year of diagnosis","Interval","Alive at Start","Died","Lost to Followup","Observed Survival (Interval)","Observed Survival (Cum)","Expected Survival (Interval)","Expected Survival (Cum)","Relative Survival (Interval)","Relative Survival (Cum)","Observed SE (Interval)","Observed SE (Cum)","Relative SE (Interval)","Relative SE (Cum)"]},"VarFormatSecList":{"Page type":{"ItemNameInDic":["0","1","2","3","4"],"ItemValueInDic":["Life Page","Summary Page","Z-Statistics Page","Period Life Page","Period Summary Page"]},"Age groups":{"ItemNameInDic":["0","1","2"],"ItemValueInDic":["00-49","45-65s","65+"]},"Breast stage":{"ItemNameInDic":["0","1","2"],"ItemValueInDic":["Localized","Regional","Distant"]},"Year of diagnosis 1975+":{"ItemNameInDic":["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],"ItemValueInDic":["1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011"]},"Interval":{"ItemNameInDic":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],"ItemValueInDic":["<1 yr","1-<2 yr","2-<3 yr","3-<4 yr","4-<5 yr","5-<6 yr","6-<7 yr","7-<8 yr","8-<9 yr","9-<10 yr","10-<11 yr","11-<12 yr","12-<13 yr","13-<14 yr","14-<15 yr","15-<16 yr","16-<17 yr","17-<18 yr","18-<19 yr","19-<20 yr","20-<21 yr","21-<22 yr","22-<23 yr","23-<24 yr","24-<25 yr","25-<26 yr","26-<27 yr","27-<28 yr","28-<29 yr","29-<30 yr","30-<31 yr","31-<32 yr","32-<33 yr","33-<34 yr","34-<35 yr","35-<36 yr"]}},"VarLabelInfo":{"FirstPart":["Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var"],"VarIndex":["1","2","2","3","3","4","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"],"SecondPart":["Name","Name","Base","Name","Base","Name","Base","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name"],"LabelValue":["Page type","Age groups","Age recode with <1 year olds","Breast stage","SEER historic stage A","Year of diagnosis 1975+","Year of diagnosis","Interval","Alive at Start","Died","Lost to Followup","Observed Survival (Interval)","Observed Survival (Cum)","Expected Survival (Interval)","Expected Survival (Cum)","Relative Survival (Interval)","Relative Survival (Cum)","Observed SE (Interval)","Observed SE (Cum)","Relative SE (Interval)","Relative SE (Cum)"]},"VarWithoutFormatItem":["Alive at Start","Died","Lost to Followup","Observed Survival (Interval)","Observed Survival (Cum)","Expected Survival (Interval)","Expected Survival (Cum)","Relative Survival (Interval)","Relative Survival (Cum)","Observed SE (Interval)","Observed SE (Cum)","Relative SE (Interval)","Relative SE (Cum)"]}]
    data3 = [{  "SystemInfo": {    "ItemNameInDic": [      "Output filename",      "Matrix filename",      "Database name"    ],    "ItemValueInDic": [      "h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.txt",      "h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.ssm",      "Incidence - SEER 18 Regs Research Data + Hurricane Katrina Impacted Louisiana Cases, Nov 2013 Sub (1973-2011 varying) - Linked To County Attributes - Total U.S., 1969-2012 Counties"    ]  },  "SessionOptionInfo": {    "ItemNameInDic": [      "Type",      "Rate filename",      "Statistic",      "SurvivalMethod",      "SurvivalBeginMonth",      "SurvivalBeginYear",      "SurvivalEndMonth",      "SurvivalEndYear",      "SurvivalVitalStatus",      "StudyCutoffDate",      "LostToFollowupDate",      "NumberOfIntervals",      "MonthsPerInterval",      "RatesDisplayedAs"    ],    "ItemValueInDic": [      "Survival",      "U.S. 1970-2009 by individual year (White, Black, Other (AI\/API), Ages 0-99, All races for Other Unspec 1991+ and Unknown)",      "Relative Survival",      "Actuarial",      "Month of diagnosis recode",      "Year of diagnosis",      "Month of follow-up recode",      "Year of follow-up recode",      "Vital status recode (study cutoff used)",      "12\/2011",      "12\/2011",      "36",      "12",      "Percents"    ]  },  "ExportOptionInfo": {    "ItemNameInDic": [      "GZipped",      "Variable format",      "File format",      "Field delimiter",      "Missing character",      "Fields with delimiter in quotes",      "Remove thousands separators",      "Flags included",      "Variable names included",      "Column Variables as Stats"    ],    "ItemValueInDic": [      "false",      "numeric",      "DOS\/Windows",      "tab",      "period",      "false",      "true",      "false",      "false",      "false"    ]  },  "VarAllInfo": {    "ItemNameInDic": [      "Var1Name",      "Var2Name",      "Var2Base",      "Var3Name",      "Var3Base",      "Var4Name",      "Var4Base",      "Var5Name",      "Var6Name",      "Var7Name",      "Var8Name",      "Var9Name",      "Var10Name",      "Var11Name",      "Var12Name",      "Var13Name",      "Var14Name",      "Var15Name",      "Var16Name",      "Var17Name",      "Var18Name"    ],    "ItemValueInDic": [      "Page type",      "Age groups",      "Age recode with <1 year olds",      "Breast stage",      "SEER historic stage A",      "Year of diagnosis 1975+",      "Year of diagnosis",      "Interval",      "Alive at Start",      "Died",      "Lost to Followup",      "Observed Survival (Interval)",      "Observed Survival (Cum)",      "Expected Survival (Interval)",      "Expected Survival (Cum)",      "Relative Survival (Interval)",      "Relative Survival (Cum)",      "Observed SE (Interval)",      "Observed SE (Cum)",      "Relative SE (Interval)",      "Relative SE (Cum)"    ]  },  "VarFormatSecList": {    "Page type": {      "ItemNameInDic": [        "0",        "1",        "2",        "3",        "4"      ],      "ItemValueInDic": [        "Life Page",        "Summary Page",        "Z-Statistics Page",        "Period Life Page",        "Period Summary Page"      ]    },    "Age groups": {      "ItemNameInDic": [        "0",        "1",        "2"      ],      "ItemValueInDic": [        "00-49",        "45-65s",        "65+"      ]    },    "Breast stage": {      "ItemNameInDic": [        "0",        "1",        "2"      ],      "ItemValueInDic": [        "Localized",        "Regional",        "Distant"      ]    },    "Year of diagnosis 1975+": {      "ItemNameInDic": [        "0",        "1",        "2",        "3",        "4",        "5",        "6",        "7",        "8",        "9",        "10",        "11",        "12",        "13",        "14",        "15",        "16",        "17",        "18",        "19",        "20",        "21",        "22",        "23",        "24",        "25",        "26",        "27",        "28",        "29",        "30",        "31",        "32",        "33",        "34",        "35",        "36"      ],      "ItemValueInDic": [        "1975",        "1976",        "1977",        "1978",        "1979",        "1980",        "1981",        "1982",        "1983",        "1984",        "1985",        "1986",        "1987",        "1988",        "1989",        "1990",        "1991",        "1992",        "1993",        "1994",        "1995",        "1996",        "1997",        "1998",        "1999",        "2000",        "2001",        "2002",        "2003",        "2004",        "2005",        "2006",        "2007",        "2008",        "2009",        "2010",        "2011"      ]    },    "Interval": {      "ItemNameInDic": [        "1",        "2",        "3",        "4",        "5",        "6",        "7",        "8",        "9",        "10",        "11",        "12",        "13",        "14",        "15",        "16",        "17",        "18",        "19",        "20",        "21",        "22",        "23",        "24",        "25",        "26",        "27",        "28",        "29",        "30",        "31",        "32",        "33",        "34",        "35",        "36"      ],      "ItemValueInDic": [        "<1 yr",        "1-<2 yr",        "2-<3 yr",        "3-<4 yr",        "4-<5 yr",        "5-<6 yr",        "6-<7 yr",        "7-<8 yr",        "8-<9 yr",        "9-<10 yr",        "10-<11 yr",        "11-<12 yr",        "12-<13 yr",        "13-<14 yr",        "14-<15 yr",        "15-<16 yr",        "16-<17 yr",        "17-<18 yr",        "18-<19 yr",        "19-<20 yr",        "20-<21 yr",        "21-<22 yr",        "22-<23 yr",        "23-<24 yr",        "24-<25 yr",        "25-<26 yr",        "26-<27 yr",        "27-<28 yr",        "28-<29 yr",        "29-<30 yr",        "30-<31 yr",        "31-<32 yr",        "32-<33 yr",        "33-<34 yr",        "34-<35 yr",        "35-<36 yr"      ]    }  },  "VarLabelInfo": {    "FirstPart": [      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var"    ],    "VarIndex": [      "1",      "2",      "2",      "3",      "3",      "4",      "4",      "5",      "6",      "7",      "8",      "9",      "10",      "11",      "12",      "13",      "14",      "15",      "16",      "17",      "18"    ],    "SecondPart": [      "Name",      "Name",      "Base",      "Name",      "Base",      "Name",      "Base",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name"    ],    "LabelValue": [      "Page type",      "Age groups",      "Age recode with <1 year olds",      "Breast stage",      "SEER historic stage A",      "Year of diagnosis 1975+",      "Year of diagnosis",      "Interval",      "Alive at Start",      "Died",      "Lost to Followup",      "Observed Survival (Interval)",      "Observed Survival (Cum)",      "Expected Survival (Interval)",      "Expected Survival (Cum)",      "Relative Survival (Interval)",      "Relative Survival (Cum)",      "Observed SE (Interval)",      "Observed SE (Cum)",      "Relative SE (Interval)",      "Relative SE (Cum)"    ]  },  "VarWithoutFormatItem": [    "Alive at Start",    "Died",    "Lost to Followup",    "Observed Survival (Interval)",    "Observed Survival (Cum)",    "Expected Survival (Interval)",    "Expected Survival (Cum)",    "Relative Survival (Interval)",    "Relative Survival (Cum)",    "Observed SE (Interval)",    "Observed SE (Cum)",    "Relative SE (Interval)",    "Relative SE (Cum)"  ]}]
    out_json = json.dumps(data3)

    return current_app.response_class(out_json, mimetype=mimetype)

@app.route('/jpsurvRest/get_form', methods = ['GET'])
def get_upload():
    # python LDpair.py rs2720460 rs11733615 EUR 38
    mimetype = 'application/json'

    print
    print 'Execute jpsurvRest/get_form1'
    print 'Gathering Variables from url'
    print
    #out_json = json.dumps(["foo", {"bar":["baz", null, 1.0, 2]}])
    #my_json = json.dumps([{"Age groups": ["0-49","50-65s","65+"], "Breast stage": ["Localized","Regional","Distant"],"Test group": ["val1","ValTwo","AnotherValue"]}])
    #data =[{'Age groups': ['0-49','50-65s','65+'], 'Breast stage': ['Localized','Regional','Distant'],'Test group': ['val1','ValTwo','AnotherValue']}]
    #data2 = [{"SystemInfo":{"ItemNameInDic":["Output filename","Matrix filename","Database name"],"ItemValueInDic":["h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.txt","h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.ssm","Incidence - SEER 18 Regs Research Data + Hurricane Katrina Impacted Louisiana Cases, Nov 2013 Sub (1973-2011 varying) - Linked To County Attributes - Total U.S., 1969-2012 Counties"]},"SessionOptionInfo":{"ItemNameInDic":["Type","Rate filename","Statistic","SurvivalMethod","SurvivalBeginMonth","SurvivalBeginYear","SurvivalEndMonth","SurvivalEndYear","SurvivalVitalStatus","StudyCutoffDate","LostToFollowupDate","NumberOfIntervals","MonthsPerInterval","RatesDisplayedAs"],"ItemValueInDic":["Survival","U.S. 1970-2009 by individual year (White, Black, Other (AI/API), Ages 0-99, All races for Other Unspec 1991+ and Unknown)","Relative Survival","Actuarial","Month of diagnosis recode","Year of diagnosis","Month of follow-up recode","Year of follow-up recode","Vital status recode (study cutoff used)","12/2011","12/2011","36","12","Percents"]},"ExportOptionInfo":{"ItemNameInDic":["GZipped","Variable format","File format","Field delimiter","Missing character","Fields with delimiter in quotes","Remove thousands separators","Flags included","Variable names included","Column Variables as Stats"],"ItemValueInDic":["false","numeric","DOS/Windows","tab","period","false","true","false","false","false"]},"VarAllInfo":{"ItemNameInDic":["Var1Name","Var2Name","Var2Base","Var3Name","Var3Base","Var4Name","Var4Base","Var5Name","Var6Name","Var7Name","Var8Name","Var9Name","Var10Name","Var11Name","Var12Name","Var13Name","Var14Name","Var15Name","Var16Name","Var17Name","Var18Name"],"ItemValueInDic":["Page type","Age groups","Age recode with <1 year olds","Breast stage","SEER historic stage A","Year of diagnosis: Year of diagnosis 1975+","Year of diagnosis","Interval","Alive at Start","Died","Lost to Followup","Observed Survival (Interval)","Observed Survival (Cum)","Expected Survival (Interval)","Expected Survival (Cum)","Relative Survival (Interval)","Relative Survival (Cum)","Observed SE (Interval)","Observed SE (Cum)","Relative SE (Interval)","Relative SE (Cum)"]},"VarFormatSecList":{"Page type":{"ItemNameInDic":["0","1","2","3","4"],"ItemValueInDic":["Life Page","Summary Page","Z-Statistics Page","Period Life Page","Period Summary Page"]},"Age groups":{"ItemNameInDic":["0","1","2"],"ItemValueInDic":["00-49","45-65s","65+"]},"Breast stage":{"ItemNameInDic":["0","1","2"],"ItemValueInDic":["Localized","Regional","Distant"]},"Year of diagnosis 1975+":{"ItemNameInDic":["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],"ItemValueInDic":["1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011"]},"Interval":{"ItemNameInDic":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],"ItemValueInDic":["<1 yr","1-<2 yr","2-<3 yr","3-<4 yr","4-<5 yr","5-<6 yr","6-<7 yr","7-<8 yr","8-<9 yr","9-<10 yr","10-<11 yr","11-<12 yr","12-<13 yr","13-<14 yr","14-<15 yr","15-<16 yr","16-<17 yr","17-<18 yr","18-<19 yr","19-<20 yr","20-<21 yr","21-<22 yr","22-<23 yr","23-<24 yr","24-<25 yr","25-<26 yr","26-<27 yr","27-<28 yr","28-<29 yr","29-<30 yr","30-<31 yr","31-<32 yr","32-<33 yr","33-<34 yr","34-<35 yr","35-<36 yr"]}},"VarLabelInfo":{"FirstPart":["Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var","Var"],"VarIndex":["1","2","2","3","3","4","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"],"SecondPart":["Name","Name","Base","Name","Base","Name","Base","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name","Name"],"LabelValue":["Page type","Age groups","Age recode with <1 year olds","Breast stage","SEER historic stage A","Year of diagnosis 1975+","Year of diagnosis","Interval","Alive at Start","Died","Lost to Followup","Observed Survival (Interval)","Observed Survival (Cum)","Expected Survival (Interval)","Expected Survival (Cum)","Relative Survival (Interval)","Relative Survival (Cum)","Observed SE (Interval)","Observed SE (Cum)","Relative SE (Interval)","Relative SE (Cum)"]},"VarWithoutFormatItem":["Alive at Start","Died","Lost to Followup","Observed Survival (Interval)","Observed Survival (Cum)","Expected Survival (Interval)","Expected Survival (Cum)","Relative Survival (Interval)","Relative Survival (Cum)","Observed SE (Interval)","Observed SE (Cum)","Relative SE (Interval)","Relative SE (Cum)"]}]
    data3 = [{  "SystemInfo": {    "ItemNameInDic": [      "Output filename",      "Matrix filename",      "Database name"    ],    "ItemValueInDic": [      "h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.txt",      "h:\\JPsurv\\DataTest\\Breast_RelativeSurvival.ssm",      "Incidence - SEER 18 Regs Research Data + Hurricane Katrina Impacted Louisiana Cases, Nov 2013 Sub (1973-2011 varying) - Linked To County Attributes - Total U.S., 1969-2012 Counties"    ]  },  "SessionOptionInfo": {    "ItemNameInDic": [      "Type",      "Rate filename",      "Statistic",      "SurvivalMethod",      "SurvivalBeginMonth",      "SurvivalBeginYear",      "SurvivalEndMonth",      "SurvivalEndYear",      "SurvivalVitalStatus",      "StudyCutoffDate",      "LostToFollowupDate",      "NumberOfIntervals",      "MonthsPerInterval",      "RatesDisplayedAs"    ],    "ItemValueInDic": [      "Survival",      "U.S. 1970-2009 by individual year (White, Black, Other (AI\/API), Ages 0-99, All races for Other Unspec 1991+ and Unknown)",      "Relative Survival",      "Actuarial",      "Month of diagnosis recode",      "Year of diagnosis",      "Month of follow-up recode",      "Year of follow-up recode",      "Vital status recode (study cutoff used)",      "12\/2011",      "12\/2011",      "36",      "12",      "Percents"    ]  },  "ExportOptionInfo": {    "ItemNameInDic": [      "GZipped",      "Variable format",      "File format",      "Field delimiter",      "Missing character",      "Fields with delimiter in quotes",      "Remove thousands separators",      "Flags included",      "Variable names included",      "Column Variables as Stats"    ],    "ItemValueInDic": [      "false",      "numeric",      "DOS\/Windows",      "tab",      "period",      "false",      "true",      "false",      "false",      "false"    ]  },  "VarAllInfo": {    "ItemNameInDic": [      "Var1Name",      "Var2Name",      "Var2Base",      "Var3Name",      "Var3Base",      "Var4Name",      "Var4Base",      "Var5Name",      "Var6Name",      "Var7Name",      "Var8Name",      "Var9Name",      "Var10Name",      "Var11Name",      "Var12Name",      "Var13Name",      "Var14Name",      "Var15Name",      "Var16Name",      "Var17Name",      "Var18Name"    ],    "ItemValueInDic": [      "Page type",      "Age groups",      "Age recode with <1 year olds",      "Breast stage",      "SEER historic stage A",      "Year of diagnosis 1975+",      "Year of diagnosis",      "Interval",      "Alive at Start",      "Died",      "Lost to Followup",      "Observed Survival (Interval)",      "Observed Survival (Cum)",      "Expected Survival (Interval)",      "Expected Survival (Cum)",      "Relative Survival (Interval)",      "Relative Survival (Cum)",      "Observed SE (Interval)",      "Observed SE (Cum)",      "Relative SE (Interval)",      "Relative SE (Cum)"    ]  },  "VarFormatSecList": {    "Page type": {      "ItemNameInDic": [        "0",        "1",        "2",        "3",        "4"      ],      "ItemValueInDic": [        "Life Page",        "Summary Page",        "Z-Statistics Page",        "Period Life Page",        "Period Summary Page"      ]    },    "Age groups": {      "ItemNameInDic": [        "0",        "1",        "2"      ],      "ItemValueInDic": [        "00-49",        "45-65s",        "65+"      ]    },    "Breast stage": {      "ItemNameInDic": [        "0",        "1",        "2"      ],      "ItemValueInDic": [        "Localized",        "Regional",        "Distant"      ]    },    "Year of diagnosis 1975+": {      "ItemNameInDic": [        "0",        "1",        "2",        "3",        "4",        "5",        "6",        "7",        "8",        "9",        "10",        "11",        "12",        "13",        "14",        "15",        "16",        "17",        "18",        "19",        "20",        "21",        "22",        "23",        "24",        "25",        "26",        "27",        "28",        "29",        "30",        "31",        "32",        "33",        "34",        "35",        "36"      ],      "ItemValueInDic": [        "1975",        "1976",        "1977",        "1978",        "1979",        "1980",        "1981",        "1982",        "1983",        "1984",        "1985",        "1986",        "1987",        "1988",        "1989",        "1990",        "1991",        "1992",        "1993",        "1994",        "1995",        "1996",        "1997",        "1998",        "1999",        "2000",        "2001",        "2002",        "2003",        "2004",        "2005",        "2006",        "2007",        "2008",        "2009",        "2010",        "2011"      ]    },    "Interval": {      "ItemNameInDic": [        "1",        "2",        "3",        "4",        "5",        "6",        "7",        "8",        "9",        "10",        "11",        "12",        "13",        "14",        "15",        "16",        "17",        "18",        "19",        "20",        "21",        "22",        "23",        "24",        "25",        "26",        "27",        "28",        "29",        "30",        "31",        "32",        "33",        "34",        "35",        "36"      ],      "ItemValueInDic": [        "<1 yr",        "1-<2 yr",        "2-<3 yr",        "3-<4 yr",        "4-<5 yr",        "5-<6 yr",        "6-<7 yr",        "7-<8 yr",        "8-<9 yr",        "9-<10 yr",        "10-<11 yr",        "11-<12 yr",        "12-<13 yr",        "13-<14 yr",        "14-<15 yr",        "15-<16 yr",        "16-<17 yr",        "17-<18 yr",        "18-<19 yr",        "19-<20 yr",        "20-<21 yr",        "21-<22 yr",        "22-<23 yr",        "23-<24 yr",        "24-<25 yr",        "25-<26 yr",        "26-<27 yr",        "27-<28 yr",        "28-<29 yr",        "29-<30 yr",        "30-<31 yr",        "31-<32 yr",        "32-<33 yr",        "33-<34 yr",        "34-<35 yr",        "35-<36 yr"      ]    }  },  "VarLabelInfo": {    "FirstPart": [      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var",      "Var"    ],    "VarIndex": [      "1",      "2",      "2",      "3",      "3",      "4",      "4",      "5",      "6",      "7",      "8",      "9",      "10",      "11",      "12",      "13",      "14",      "15",      "16",      "17",      "18"    ],    "SecondPart": [      "Name",      "Name",      "Base",      "Name",      "Base",      "Name",      "Base",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name",      "Name"    ],    "LabelValue": [      "Page type",      "Age groups",      "Age recode with <1 year olds",      "Breast stage",      "SEER historic stage A",      "Year of diagnosis 1975+",      "Year of diagnosis",      "Interval",      "Alive at Start",      "Died",      "Lost to Followup",      "Observed Survival (Interval)",      "Observed Survival (Cum)",      "Expected Survival (Interval)",      "Expected Survival (Cum)",      "Relative Survival (Interval)",      "Relative Survival (Cum)",      "Observed SE (Interval)",      "Observed SE (Cum)",      "Relative SE (Interval)",      "Relative SE (Cum)"    ]  },  "VarWithoutFormatItem": [    "Alive at Start",    "Died",    "Lost to Followup",    "Observed Survival (Interval)",    "Observed Survival (Cum)",    "Expected Survival (Interval)",    "Expected Survival (Cum)",    "Relative Survival (Interval)",    "Relative Survival (Cum)",    "Observed SE (Interval)",    "Observed SE (Cum)",    "Relative SE (Interval)",    "Relative SE (Cum)"  ]}]
    out_json = json.dumps(data3)

    return current_app.response_class(out_json, mimetype=mimetype)

@app.route('/jpsurvRest/loadform', methods = ['GET'])
def load():
    jsondata = '{"Age groups": ["0-49","50-65s","65+"],"Breast stage": ["Localized","Regional","Distant"],"Test group": ["val1","ValTwo","AnotherValue"]}'
    return json.dump(jsondata)

@app.route('/jpsurvRest/stage1_upload', methods=['POST'])
def stage1_upload():
    #print "Processing upload"
    print OKBLUE+UNDERLINE+HEADER + BOLD + "****** Stage 1: UPLOAD BUTTON ***** " + ENDC
    tokenId = request.args.get('tokenId', False);
    print (BOLD + "****** Stage 1: tokenId = %s" + ENDC) % (tokenId)

    for k, v in request.args.iteritems():
        print "var: %s = %s" % (k, v)

    file = request.files['file_control']
    if file and file.filename:
        filename = secure_filename(file.filename)
        file.save(os.path.join(UPLOAD_DIR, filename))
        file_control_filename = filename
        print "Saving file_control: %s" % file_control_filename
    file = request.files['file_data']
    if file and file.filename:
        filename = secure_filename(file.filename)
        file.save(os.path.join(UPLOAD_DIR, filename))
        file_data_filename = filename
        print "Saving file_data: %s" % file_data_filename

    if(request.files['file_control'] == ''):
        print "file_control not assigned"
    if(request.files['file_data'] == ''):
        print "file_data not assigned"

    #Now that the files are on the server RUN the RCode
    rSource = robjects.r('source')

    print HEADER + "****** STARTING HERE ***** " + ENDC

    #PRINT FILE_CONTROL
    file_control = os.path.join(UPLOAD_DIR, file_control_filename)
    print "file_control"
    fo = open(file_control, "r+")
    str = fo.read(250);
    print OKBLUE+"Read String is : ", str
    print ENDC
    print
    fo.close()

    #PRINT FILE_DATA
    file_data = os.path.join(UPLOAD_DIR, file_data_filename)
    print "file_data"
    fo = open(file_control, "r+")
    str = fo.read(500);
    print WARNING+"Read String is : ", str
    print ENDC
    fo.close()

    #Init the R Source
    rSource = robjects.r('source')
    rSource('./JPSurvWrapper.R')

    # Next two lines execute the R Program
    getDictionary = robjects.globalenv['getDictionary']
    rStrVector = getDictionary(file_control_filename, UPLOAD_DIR, tokenId)
    #Convert R StrVecter to tuple to str

    #keyId = "".join(tuple(rStrVector))
    output_filename = "form-%s.json" % tokenId

    print output_filename
    #PRINT output_file
    r_output_file = os.path.join(UPLOAD_DIR, output_filename)
    print "R output_file"
    fo = open(r_output_file, "r+")
    str = fo.read(500);
    print BOLD+"Read String is : ", str
    print ENDC
    fo.close()

    print "CLOSE FILE %s" % output_filename
    print OKGREEN +"************ END HERE EXIT ********" + ENDC

    #print "json string >> "+str(jsondata[0]);
    status = "uploaded"

    return_url = "%s/jpsurv?file_control_filename=%s&file_data_filename=%s&output_filename=%s&status=%s&tokenId=%s" % (request.url_root, file_control_filename, file_data_filename, output_filename, status, tokenId)
    print return_url
    return redirect(return_url)

@app.route('/jpsurvRest/stage2_calculate', methods=['GET'])
def stage2_calculate():

    print UNDERLINE+HEADER + "****** CALCULATE BUTTON ***** " + ENDC

    jpsurvDataString = request.args.get('jpsurvData', False);
    print BOLD+"**** jpsurvDataString ****"+ENDC
    print jpsurvDataString
    jpsurvData = json.loads(jpsurvDataString)
    print BOLD+"**** jpsurvData ****"+ENDC
    for key, value in jpsurvData.iteritems():
        print "var: %s = %s" % (key, value)
    #Init the R Source
    rSource = robjects.r('source')
    rSource('./JPSurvWrapper.R')

    print BOLD+OKBLUE+"**** Calling getFittedResults ****"+ENDC
    # Next two lines execute the R Program
    #getFittedResultJSON = robjects.globalenv['getFittedResultJSON']
    #rStrVector = getFittedResultJSON(UPLOAD_DIR, jpsurvDataString)
    getFittedResultWrapper = robjects.globalenv['getFittedResultWrapper']
    rStrVector = getFittedResultWrapper(UPLOAD_DIR, jpsurvDataString)
    apcDataString = "".join(tuple(rStrVector))
    print apcDataString
    #return json.dumps("{\"start.year\":[1975,2001],\"end.year\":[2001,2011],\"estimate\":[-0.0167891169889347,-0.0032678676219079]}")
    print json.dumps(apcDataString)

    return json.dumps(apcDataString)

@app.route('/jpsurvRest/stage3_plot', methods=['GET'])
def stage3_plot():
    print UNDERLINE+HEADER + "****** PLOT BUTTON ***** " + ENDC
    print "Plotting ..."


    for k, v in request.args.iteritems():
        print "var: %s = %s" % (k, v)
    #name = request.get_json().get('jpsurvData', '')
    #print name
    jpsurvDataString = request.args.get('jpsurvData', True);
    print BOLD+"**** jpsurvDataString ****"+ENDC
    print jpsurvDataString
    jpsurvData = json.loads(jpsurvDataString)
    print BOLD+"**** jpsurvData ****"+ENDC
    for key, value in jpsurvData.iteritems():
        print "var: %s = %s" % (key, value)

    #Init the R Source
    rSource = robjects.r('source')
    rSource('./JPSurvWrapper.R')

    print BOLD+OKBLUE+"**** Calling getGraph ****"+ENDC
    getGraphWrapper = robjects.globalenv['getGraphWrapper']
    getGraphWrapper(UPLOAD_DIR, jpsurvDataString)

    return "done"

@app.route('/jpsurvRest/stage4_link', methods=['GET'])
def stage4_link():
    print UNDERLINE+HEADER + "****** LINK ***** " + ENDC
    print "Linking ..."


    #for k, v in request.args.iteritems():
    #    print "var: %s = %s" % (k, v)
    #name = request.get_json().get('jpsurvData', '')
    #print name
    jpsurvDataString = request.args.get('jpsurvData', True);
    #print BOLD+"**** jpsurvDataString ****"+ENDC
    #print jpsurvDataString
    jpsurvData = json.loads(jpsurvDataString)
    print BOLD+"**** jpsurvData ****"+ENDC
    for key, value in jpsurvData.iteritems():
        print "var: %s = %s" % (key, value)

    #Init the R Source
    rSource = robjects.r('source')
    rSource('./JPSurvWrapper.R')

    print BOLD+OKGREEN+"**** Calling getLink ****"+ENDC
    getDownloadOutputWrapper = robjects.globalenv['getDownloadOutputWrapper']
    rStrVector = getDownloadOutputWrapper(UPLOAD_DIR, jpsurvDataString)

    downloadLinkFileName = "".join(tuple(rStrVector))
    print downloadLinkFileName
    link = "{\"link\":\"%s\"}" % (downloadLinkFileName)
    #return json.dumps("{\"start.year\":[1975,2001],\"end.year\":[2001,2011],\"estimate\":[-0.0167891169889347,-0.0032678676219079]}")
    print json.dumps(link)

    return json.dumps(link)


import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9910", help="Sets the Port")

    args = parser.parse_args()
    port_num = int(args.port_number);

    hostname = gethostname()
    app.run(host='0.0.0.0', port=port_num, debug = True)

