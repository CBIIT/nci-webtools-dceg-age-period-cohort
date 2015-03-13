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

#exit()
#END TESTING AREA

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

@app.route('/jpsurvRest/get_form', methods = ['GET'])
def get_upload():
    # python LDpair.py rs2720460 rs11733615 EUR 38
    mimetype = 'application/json'

    print
    print 'Execute jpsurvRest/get_form'
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

@app.route('/jpsurvRest/upload', methods=['POST'])
def upload():
    print "Processing upload"

    print request.url_root  # prints "http://domain1.com/"
    print request.headers['Host']  # prints "domain1.com"
    if request.method == 'POST':
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
    print request.files['file_control']
    print request.files['file_data']

    if(request.files['file_control'] == ''):
        print "file_control not assigned"
    if(request.files['file_data'] == ''):
        print "file_data not assigned"

    #Now that the files are on the server RUN the RCode
    rSource = robjects.r('source')

    print HEADER + "****** STARTING HERE ***** " + ENDC
   # path = "/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv"
    #Use next line to

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
    rStrVector = getDictionary(file_control_filename, UPLOAD_DIR)
    #Convert R StrVecter to tuple to str

    keyId = "".join(tuple(rStrVector))
    output_file = "output-%s.json" % keyId
    print output_file
    #PRINT output_file
    r_output_file = os.path.join(UPLOAD_DIR, output_file)
    print "R output_file"
    fo = open(r_output_file, "r+")
    str = fo.read(500);
    print BOLD+"Read String is : ", str
    print ENDC
    fo.close()

    print "CLOSE FILE %s" % output_file
    print OKGREEN +"************ END HERE EXIT ********" + ENDC

    #print "json string >> "+str(jsondata[0]);
    status = "OK"

    return_url = "%s/jpsurv?file_control_filename=%s&file_data_filename=%s&output_file=%s&keyId=%s&status=%s" % (request.url_root, file_control_filename, file_data_filename, output_file, keyId, status)
    print return_url
    return redirect(return_url)

@app.route('/jpsurvRest/calculate', methods=['GET'])
def calculate():

    app.logger.debug('A value for debugging')
    #app.logger.warning('A warning occurred (%d apples)', 42)
    #app.logger.error('An error occurred')
    print UNDERLINE+HEADER + "****** CALCULATE BUTTON ***** " + ENDC
    print "Processing calculate"
    print "What does REQUEST LOOK LIKE?"
    print "What can we do with it?"
    print dir(request);
    print dir(request.args);
    print type(request.args);

    for k, v in request.args.iteritems():
        print "var: %s = %s" % (k, v)
    obj = request.args.get('obj', False);
    key_id = request.args.get('key_id', False);

    print BOLD+"**** obj ****"+ENDC
    print type(obj)
    print dir(obj)
    print BOLD+OKBLUE+"**** key_id ****"+ENDC
    print type(key_id)
    print dir(key_id)

    #jdata = json.loads(obj.decode(encoding='UTF-8'))
    jdata = json.loads(obj)
    print BOLD+"**** jdata ****"+ENDC
    print type(jdata)
    print dir(jdata)
    for key, value in jdata.iteritems():
        print "var: %s = %s" % (key, value)

    #Init the R Source
    rSource = robjects.r('source')
    rSource('./JPSurvWrapper.R')

    print BOLD+OKBLUE+"**** Calling getFittedResult ****"+ENDC
    # Next two lines execute the R Program
    getFittedResult = robjects.globalenv['getFittedResult']
    rStrVector = getFittedResult(UPLOAD_DIR, jdata['seerFilePrefix'], jdata['yearOfDiagnosisVarName'], jdata['yearOfDiagnosisRange'], jdata['allVars'], jdata['cohortVars'], jdata['cohortValues'], jdata['covariateVars'], jdata['numJP'], jdata['keyId'])

    return "Hello"

@app.route('/jpsurvRest/stage2_calculate', methods=['GET'])
def stage2_calculate():
    print "Stage 2:  Calling getFittedResult"
    command = "Rscript getFittedResult.R"
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
    process.wait()
    print process.returncode
    with open ("Breast_RelativeSurvival.output.json", "r") as myfile:
        data=myfile.read().replace('\n', '')

    return json.dumps(data)

@app.route('/jpsurvRest/apc', methods=['GET'])
def apc():
    print "apc called"
    print "output"

    print json.dumps({"start.year":[1975,2001],"end.year":[2001,2011],"estimate":[-0.0167891169889346,-0.00326786762190838]})

    return json.dumps({"start.year":[1975,2001],"end.year":[2001,2011],"estimate":[-0.0167891169889346,-0.00326786762190838]})

@app.route('/jpsurvRest/path', methods=['GET'])
def path():
    #Init the R Source
    print "Python about to execute some R code"
    rSource = robjects.r('source')
    rSource('./path.R')
    printLibPath = robjects.globalenv['printLibPath']
    printLibPath()

    return "TaDa"

@app.route('/jpsurvRest/stage1_upload', methods=['POST'])
def stage1_upload():
    print "Processing upload"

    print request.url_root  # prints "http://domain1.com/"
    print request.headers['Host']  # prints "domain1.com"
    #Saving the dictionary file and data file to server
    if request.method == 'POST':
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
    print request.files['file_control']
    print request.files['file_data']

    if(request.files['file_control'] == ''):
        print "file_control not assigned"
    if(request.files['file_data'] == ''):
        print "file_data not assigned"

    #Now that the files are on the server RUN the RCode
    rSource = robjects.r('source')

    print HEADER + "****** STARTING HERE ***** " + ENDC
   # path = "/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv"
    #Use next line to

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
    rStrVector = getDictionary(file_control_filename, UPLOAD_DIR)
    #Convert R StrVecter to tuple to str

    keyId = "".join(tuple(rStrVector))
    output_file = "output-%s.json" % keyId
    print output_file
    #PRINT output_file
    r_output_file = os.path.join(UPLOAD_DIR, output_file)
    print "R output_file"
    fo = open(r_output_file, "r+")
    str = fo.read(500);
    print BOLD+"Read String is : ", str
    print ENDC
    fo.close()

    print "CLOSE FILE %s" % output_file
    print OKGREEN +"************ END HERE EXIT ********" + ENDC

    #print "json string >> "+str(jsondata[0]);
    status = "OK"

    return_url = "%s/jpsurv?file_control_filename=%s&file_data_filename=%s&output_file=%s&keyId=%s&status=%s" % (request.url_root, file_control_filename, file_data_filename, output_file, keyId, status)
    print return_url
    return redirect(return_url)

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9982", help="Sets the Port")
    # Default port is production value; prod,stage,dev = 9982, sandbox=9983
    args = parser.parse_args()
    port_num = int(args.port_number);

    hostname = gethostname()
    app.run(host='0.0.0.0', port=port_num, debug = True)

