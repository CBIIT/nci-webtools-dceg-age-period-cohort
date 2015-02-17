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

UPLOAD_DIR = os.path.join(os.path.dirname(__file__), 'tmp')
print UPLOAD_DIR

app = Flask(__name__, static_folder='', static_url_path='/')
#app.debug = True

#TESTING AREA

#exit()
#END TESTING AREA

#app = Flask(__name__, static_folder='static', static_url_path='/static')

def what_is_this(sv):
    print "what_is_this?"
    print type(sv)
    print dir(sv)
    print ""
    print sv
    print type(sv)
    print sv[0]
    print str(sv)
    print len(sv)
    print format(tuple(sv))
    print type(tuple(sv))
    print "".join(tuple(sv))

sv = robjects.StrVector('output-92383.json')
what_is_this(sv)


def random_with_N_digits(n):
    range_start = 10**(n-1)
    range_end = (10**n)-1
    return randint(range_start, range_end)

@app.route('/')
def hello_world():
    return 'Hello World!'

# copy output files from tools' tmp directory to apache tmp directory
def copy_output_files(reference):
    apache_root = "/analysistools/"
    # check if URL contains the keyword sandbox
    if 'sandbox' in request.url_root:
        apache_root = "/analysistools-sandbox/"

    apache_tmp_dir = apache_root+"public_html/apps/LDlink/tmp/"

    # Ensure apache tmp directory exists
    if not os.path.exists(apache_tmp_dir):
	os.makedirs(apache_tmp_dir)

    #copy *<reference_no>.* to htodocs
    os.system("cp "+ tmp_dir+"*"+reference+".* "+apache_tmp_dir);

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

@app.route('/jpsurvRest/hello', methods = ['GET'])

def hello():
    print
    print 'Execute hello'
    print 'Got it work.  How fun.'
    print 'Get Variables from url'
    print

    return 'Hello, We are the world.'

@app.route('/LDlinkRest/ldmatrix', methods = ['GET'])
def ldmatrix():
    # python LDmatrix.py snps.txt EUR 5
    #http://analysistools-sandbox.nci.nih.gov/LDlinkRest/ldmatrix?pop=EUR&reference=5&snp=sr3
    #http://analysistools-sandbox.nci.nih.gov/LDlinkRest/ldmatrix?filename=get+from+input&pop=LWK%2BGWD&reference=76178
    print
    print 'Execute ldmatrix'
    print 'Gathering Variables from url'

    snps = request.args.get('snps', False)
    pop = request.args.get('pop', False)
    reference = request.args.get('reference', False)
    print 'snps: ' + snps
    print 'pop: ' + pop
    print 'request: ' + reference

    snplst = tmp_dir+'snps'+reference+'.txt'
    print 'snplst: '+snplst

    f = open(snplst, 'w')
    f.write(snps)
    f.close()

    out_script,out_div = calculate_matrix(snplst,pop,reference)

    copy_output_files(reference)
    return out_script+"\n "+out_div


@app.route('/jpsurvRest/loadform', methods = ['GET'])
def load():
    jsondata = '{"Age groups": ["0-49","50-65s","65+"],"Breast stage": ["Localized","Regional","Distant"],"Test group": ["val1","ValTwo","AnotherValue"]}'
    return json.dump(jsondata)


@app.route('/src-ck/jpsurv/form_upload', methods = ['GET'])
def ldhap():

    print
    print 'Execute ldhap'

    print 'Gathering Variables from url'

    snps = request.args.get('snps', False)
    pop = request.args.get('pop', False)
    reference = request.args.get('reference', False)
    print 'snps: ' + snps
    print 'pop: ' + pop
    print 'request: ' + reference

    snplst = tmp_dir+'snps'+reference+'.txt'
    print 'snplst: '+snplst

    f = open(snplst, 'w')
    f.write(snps)
    f.close()

    out_json = calculate_hap(snplst,pop,reference)
    copy_output_files(reference)

    return out_json


@app.route('/LDlinkRest/test', methods=['GET', 'POST'])
def test():
    print 'In /LDlinkRest/test'
    print 'request.headers[Content-Type]'
    print request.headers['Content-Type']
    print ''
    print 'request.data'
    print request.data
    print 'request.args'
    print json.dumps(request.args)

    print 'request.files'
    print request.files

    print 'request.method'
    print request.method

    print
    print 'Execute ldmatrix'
    print 'Gathering Variables from url'
    snps = request.args.get('snps', False)
    #filename = request.args.get('filename', False)
    pop = request.args.get('pop', False)
    reference = request.args.get('reference', False)
    print 'snp: ' + snp
    print 'pop: ' + pop
    print 'request: ' + reference
    print
    snplst = 'snps2.txt'

    if request.headers['Content-Type'] == 'text/plain':
        return "Text Message: " + request.data

    elif request.headers['Content-Type'] == 'application/json':
        return "JSON Message: " + json.dumps(request.json)

    elif request.headers['Content-Type'] == 'application/octet-stream':
        f = open('./binary', 'wb')
        f.write(request.data)
        f.close()
        return "Binary message written!"
    elif request.headers['Content-Type'] == 'multipart/form-data':
        return 'multipart/form-data'
    elif request.headers['Content-Type'] == 'application/x-www-form-urlencoded':
        return 'application/x-www-form-urlencoded'

    else:
        return "415 Unsupported Media Type ;)"


@app.route('/jpsurvRest/upload', methods=['POST'])
def upload():
    print "Processing upload"
    path = "/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"
    path = UPLOAD_DIR
    print path
    print dir(request)
    print "About to call upload(), how cool."
    print "*********************************"
    print dir(request.files)
    print "type(requset.files)"
    print type(request.files)

    for key, value in request.files.iteritems() :
        print key
        print value
        print type(value)
    print ""
    for key in request.files:
        val = '%('+key+')s'
        print key, val % request.files

    print "request.args[name]"
    #print request.files[0]
    print "*********************************"
    return "hello"
    #return upload(request)

    if request.method == 'POST':
        file = request.files['file_control']
        if file and file.filename:
            filename = secure_filename(file.filename)
            print filename
            file.save(os.path.join(path, filename))
            #return redirect(url_for('uploaded_file', filename=filename))
            #return "<h2>File Uploaded</h2>"

    #rSource = robjects.r('source')
    #rSource('./JPSurvWrapper.R')
    #r_getname_getData = robjects.globalenv['getDataJSON']

    #rec = Photo(filename=filename, user=g.user.id)
    #rec.store()
    #flash("Photo saved.")
    #return redirect(url_for('show', id=rec.id))
    #return render_template('upload.html')
    return redirect("http://analysistools-dev.nci.nih.gov/jpsurv-ck/")

@app.route('/jpsurvRest/upload2', methods=['POST'])
def upload2():
    print "Processing upload"

    print request.url_root  # prints "http://domain1.com/"
    print request.headers['Host']  # prints "domain1.com"

    #path = "/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"
    path = UPLOAD_DIR
    print "Upload path: %s" % path
    if request.method == 'POST':
        file = request.files['file_control']
        if file and file.filename:
            filename = secure_filename(file.filename)
            file.save(os.path.join(path, filename))
            file_control_filename = filename
            print "Saving file_control: %s" % file_control_filename
        file = request.files['file_data']
        if file and file.filename:
            filename = secure_filename(file.filename)
            file.save(os.path.join(path, filename))
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

    print "****** STARTING HERE ***** "

    path = "/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv"
    out_path = os.path.join(path, 'tmp')
    #Use next line to
    #out_path = "%s/tmp/" % UPLOAD_DIR

    #Init the R Source
    rSource = robjects.r('source')
    rSource('./JPSurvWrapper.R')

    # Next two lines execute the R Program
    getDictionary = robjects.globalenv['getDictionary']
    rStrVector = getDictionary(file_control_filename, out_path)
    #Convert R StrVecter to tuple to str
    output_file = "".join(tuple(rStrVector))

    print output_file

    print "CLOSE FILE %s" % output_file
    print "************ EXIT ********"

    #print "json string >> "+str(jsondata[0]);
    status = "OK"

    return_url = "%s/jpsurv?file_control_filename=%s&file_data_filename=%s&output_file=%s&status=%s" % (request.url_root, file_control_filename, file_data_filename, output_file, status)
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

