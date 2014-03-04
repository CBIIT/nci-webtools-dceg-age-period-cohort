#!flask/bin/python
from flask import Flask, Response, abort, request, make_response, url_for, jsonify
#from flask.ext.httpauth import HTTPBasicAuth
#from flask.ext.jsonpify import jsonify
from functools import wraps
from flask import current_app

import rpy2.robjects as robjects
import cgi
import shutil
import os
from xml.sax.saxutils import escape, unescape
from socket import gethostname

#app = Flask(__name__, static_url_path = "/web/index.html")
#auth = HTTPBasicAuth()

app = Flask(__name__)

@app.route('/')
@app.route('/index.html')
def home():
    return render_template('/index.html')


 
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

def parseString(inString):
      parsedstring = "{";
      splitStringL = inString.split("\n")
      splitString  = splitStringL [1::1];
      SPLITSTRLENGTH = len(splitString)
      trackSplitString = 0;
      for row in splitString:
         if trackSplitString < SPLITSTRLENGTH-1:
            parsedstring += "\"" + str(trackSplitString ) + "\"" + ":" + "{";
         splitrow = row.split();
         L = splitrow [1::1];
         LSTRLENGTH = len(L)
         trackLString = 0;
         for eachRowItem in L:
              if trackLString < LSTRLENGTH-1:
                 parsedstring += "\"" + str(trackLString) + "\"" + ":" + str(eachRowItem) + ",";
              else:
                 if trackSplitString < SPLITSTRLENGTH-2:
                    parsedstring += "\"" + str(trackLString) + "\"" + ":" + str(eachRowItem) +"},"
                 else:
                    parsedstring += "\"" + str(trackLString) + "\"" + ":" + str(eachRowItem) +"}"
              trackLString += 1;
         trackSplitString += 1;
      parsedstring += "}"
      return parsedstring;

@app.route('/todo/api/v1.0/checkInput')
def checkInput():
    #print request.query_string
    arrayVal = parseQueryString(request.query_string)
    print arrayVal[0];
    print arrayVal[1];
    print arrayVal[2];
    print arrayVal[3];
    return arrayVal

@app.route('/todo/api/v1.0/checkValue/<inputValues>')
def checkValue(inputValues):
    print inputValues
    return str(inputValues);

def parseQueryString(inString):
      replacedString =  inString.replace('=','<-')
      #splitStringL = replacedString.split("),")
      splitStringL = replacedString.split("&")
      return splitStringL


@app.route('/DrawROC', methods = ['GET'])
def DrawROC():
    print "Request string   >> "+request.query_string
    inputValues = request.query_string.split("&")
    deltaInputValue = "Delta<-"+str(inputValues[0])
    print "delta Input Value  >> "+deltaInputValue
    sourceReturnin1 = robjects.r(deltaInputValue)
    sourceReturn2 = robjects.r('''source('DrawROC.R')''')
    r_getname = robjects.globalenv['DrawROC']
    r_getname_delta = robjects.globalenv['Delta']
    sourceReturnValue = r_getname(r_getname_delta)
    print "  >> " + str(sourceReturnValue)
    return str(sourceReturnValue)


def drawGraph(uniquekey, specMin, specMax, deltaValue):
    uqInputValue = "uniquekey<-"+str(uniquekey)
    deltaInputValue = "Delta<-"+str(deltaValue)
    specMinInputValue = "specmin<-"+str(specMin)
    specMaxInputValue = "specmax<-"+str(specMax)
    print "delta Input Value  >> "+deltaInputValue
    sourceReturnin4 = robjects.r(uqInputValue)
    sourceReturnin1 = robjects.r(deltaInputValue)
    sourceReturnin2 = robjects.r(specMinInputValue)
    sourceReturnin3 = robjects.r(specMaxInputValue)
    sourceReturn2 = robjects.r('''source('DrawROC.R')''')
    r_getname = robjects.globalenv['DrawROC']
    r_getname_delta = robjects.globalenv['Delta']
    r_getname_spec_min = robjects.globalenv['specmin']
    r_getname_spec_max = robjects.globalenv['specmax']
    r_getname_unique_key = robjects.globalenv['uniquekey']
    sourceReturnValue = r_getname(r_getname_unique_key, r_getname_spec_min, r_getname_spec_max, r_getname_delta)
    print "  >> " + str(sourceReturnValue)
    return str(sourceReturnValue)

def setRWorkingDirectory():
    sourceReturn1 = robjects.r("path")
    return ""

def parseDelta(inString):
      splitStringL = inString.split("\n")
      SPLITSTRLENGTH = len(splitStringL)
      print "SPLITSTRLENGTH  "+str(SPLITSTRLENGTH)
      print "splitStringL  "+str(splitStringL[SPLITSTRLENGTH-2])
      splitStringDelta = str(splitStringL[SPLITSTRLENGTH-2])
      splitStringDelta2 = splitStringDelta.split(" ")
      SPLITSTRLENGTHDELTA = len(splitStringDelta2)
      print "***** DELTA  "+str(splitStringDelta2[SPLITSTRLENGTHDELTA-1])
      return str(splitStringDelta2[SPLITSTRLENGTHDELTA-1])

@app.route('/json/DeltaSpecPpv', methods = ['GET'])
def deltaSpecPpv():
    print " deltaSpecPpv >> " + request.query_string + "EDDDDDDD\n";
    replacedString =  request.query_string.replace('=','<-')
    replacedString1 =  replacedString.replace('%2C',',')
    inputValues = replacedString1.split("&")
    print "  >> " + replacedString
    print "  >> " + replacedString1
    print " input Values  >> " + str(inputValues);
    #sourceReturn = robjects.r('''source('input.R')''')
    #sourceReturnin1 = robjects.r('cases<-c(4,0.1,100)')
    sourceReturnin1 = robjects.r(str(inputValues[0]))
    #sourceReturnin2 = robjects.r('controls<-c(1,0.1,200)')
    sourceReturnin2 = robjects.r(str(inputValues[1]))
    #sourceReturnin3 = robjects.r('specificity<-c(0.8,0.9,0.95,0.99,0.995)')
    sourceReturnin3 = robjects.r(str(inputValues[2]))
    #sourceReturnin4 = robjects.r('prevalence<-c(0.1,0.01,0.001,0.0001,0.00001,0.000001)')
    sourceReturnin4 = robjects.r(str(inputValues[3]))
    sourceReturn2 = robjects.r('''source('DeltaSpecPpv.R')''')
    r_getname = robjects.globalenv['deltaspecppv']
    r_getname_cases = robjects.globalenv['cases']
    r_getname_controls = robjects.globalenv['controls']
    r_getname_specificity = robjects.globalenv['specificity']
    r_getname_prevalence = robjects.globalenv['prevalence']
    sourceReturnValue = r_getname(r_getname_cases,r_getname_controls,r_getname_specificity,r_getname_prevalence)
    dataf = robjects.DataFrame(sourceReturnValue)
    returnString = "{\"SensitivityGivenSpecificity\":["
    returnString  += parseString(str(dataf[1-0]))
    returnString  += "],\"PPV\":["
    returnString += parseString(str(dataf[2]))
    returnString  +="],\"cNPV\":["
    returnString += parseString(str(dataf[3]))
    returnString  += "],\"PPVcNPV\":["
    returnString += parseString(str(dataf[4]))
    returnString  += "],\"ProgramBased\":["
    returnString += parseString(str(dataf[5]))
    returnString  += "],\"PPVBased\":["
    returnString += parseString(str(dataf[6]))
    returnString += "],\"SensitivityBased\":["
    returnString += parseString(str(dataf[7]))
    returnString += "], \"DominatedByRareDisease\" :[ "
    returnString += parseString(str(dataf[8]))
    returnString += "]}"
 
    # Generate the GRAPH
    drawGraph(str(inputValues[4]))
    # Move file to static directory
#    shutil.copyfile("rplot.png","web/tmp/rplot.png")
    print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print returnString  
    print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    return jsonify({"returnstring" : returnString})
    #return returnString


@app.route('/jsonp/DeltaSpPpv', methods = ['GET'])
@jsonp
def deltaSpPpv():
    print " deltaSpPpv >> " + request.query_string + "EDDDDDDD\n";
    replacedString =  request.query_string.replace('=','<-')
    replacedString1 =  replacedString.replace('%2C',',')
    inputValues = replacedString1.split("&")
    print "  >> " + replacedString
    print "  >> " + replacedString1
    print " input Values  >> " + str(inputValues);
    callBackFunction = inputValues[0].split("<-")
    print " callback function 0 >> " + callBackFunction[0];
    print " callback function 1 >> " + callBackFunction[1];
    #sourceReturn = robjects.r('''source('input.R')''')
    #sourceReturnin1 = robjects.r('cases<-c(4,0.1,100)')
    sourceReturnin1 = robjects.r(str(inputValues[1]))
    #sourceReturnin2 = robjects.r('controls<-c(1,0.1,200)')
    sourceReturnin2 = robjects.r(str(inputValues[2]))
    #sourceReturnin3 = robjects.r('specificity<-c(0.8,0.9,0.95,0.99,0.995)')
    sourceReturnin3 = robjects.r(str(inputValues[3]))
    #sourceReturnin4 = robjects.r('prevalence<-c(0.1,0.01,0.001,0.0001,0.00001,0.000001)')
    sourceReturnin4 = robjects.r(str(inputValues[4]))
    sourceReturn2 = robjects.r('''source('DeltaSpecPpv.R')''')
    sourceReturn3 = robjects.r('''source('TestCCData.R')''')
    r_getname_2 = robjects.globalenv['TestCCData']
    r_getname = robjects.globalenv['deltaspecppv']
    r_getname_cases = robjects.globalenv['cases']
    r_getname_controls = robjects.globalenv['controls']
    r_getname_specificity = robjects.globalenv['specificity']
    r_getname_prevalence = robjects.globalenv['prevalence']
    sourceReturnValue = r_getname(r_getname_cases,r_getname_controls,r_getname_specificity,r_getname_prevalence)
    sourceReturnValue2 = r_getname_2(r_getname_cases,r_getname_controls)
    graphAvail="{\"0\":0}";
    dataf = robjects.DataFrame(sourceReturnValue)
    calculatedDelta = parseDelta(str(dataf[0]))
    if (str(sourceReturnValue2[0]) == "True"):
       print " value 0 is true ";
       graphAvail="{\"0\":1}";
       #dataf = robjects.DataFrame(sourceReturnValue)
       print " print dataframe "+str(dataf)
       #returnString = str(callBackFunction[1])+"("
       returnString = "{\"SensitivityGivenSpecificity\":["
       returnString  += parseString(str(dataf[1-0]))
       returnString  += "],\"PPV\":["
       returnString += parseString(str(dataf[2]))
       returnString  +="],\"cNPV\":["
       returnString += parseString(str(dataf[3]))
       returnString  += "],\"PPVcNPV\":["
       returnString += parseString(str(dataf[4]))
       returnString  += "],\"ProgramBased\":["
       returnString += parseString(str(dataf[5]))
       returnString  += "],\"PPVBased\":["
       returnString += parseString(str(dataf[6]))
       returnString += "],\"SensitivityBased\":["
       returnString += parseString(str(dataf[7]))
       returnString += "], \"DominatedByRareDisease\" :[ "
       returnString += parseString(str(dataf[8]))
       returnString += "], \"DrawGraph\" :[ "
       returnString += graphAvail
       returnString += "], \"calculatedDelta\" :[ "
       returnString += "{\"0\":"+calculatedDelta
       returnString += "}]}"
    else:
       print " value 0 is false ";
       graphAvail="{\"0\":0}";
       returnString = "{\"DrawGraph\" :["
       returnString += graphAvail
       returnString += "]}"
    #returnString += "]})"
 
    # clear rplot.png from the directory
    if os.path.isfile("rplot.png"):
        os.remove("rplot.png")
    if os.path.isfile("web/tmp/rplot.png"):
        os.remove("web/tmp/rplot.png")
    else:
        print "no rplot.png file "
    # Generate the GRAPH
    print " input Values 5  >> " + str(inputValues[5]);
    print " input Values 6 >> " + str(inputValues[6]);
    print " input Values 7  >> " + str(inputValues[7]);
    print " input Values 8  >> " + str(inputValues[8]);
    drawGraph(str(inputValues[8]),str(inputValues[6]),str(inputValues[7]),calculatedDelta)
    # Move file to static directory
    keyUnique = inputValues[8].split("<-")
#    shutil.copyfile(str(keyUnique[1])+"rplot.png","web/tmp/"+str(keyUnique[1])+"rplot.png")
    print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print " callback function 1 >> " + callBackFunction[1];
    print returnString  
    print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    return jsonify({"returnstring" : returnString})
    #return returnString

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="8080", help="Sets the Port") 
    # Default port is production value; prod,stage,dev = 8080, sandbox=8780
    args = parser.parse_args()
    port_num = int(args.port_number);

    hostname = gethostname()
    app.run(host=hostname, port=port_num,debug = True)
