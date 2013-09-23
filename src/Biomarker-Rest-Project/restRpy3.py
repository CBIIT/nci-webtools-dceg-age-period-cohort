#!flask/bin/python
from flask import Flask, abort, request, make_response, url_for, jsonify
#from flask.ext.httpauth import HTTPBasicAuth
#from flask.ext.jsonpify import jsonify
from functools import wraps
#from flask import request, current_app

import rpy2.robjects as robjects
import cgi
import shutil
from xml.sax.saxutils import escape, unescape

app = Flask(__name__, static_url_path = "")
#auth = HTTPBasicAuth()
 

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


def drawGraph(deltaValue):
    deltaInputValue = "Delta<-"+str(deltaValue)
    print "delta Input Value  >> "+deltaInputValue
    sourceReturnin1 = robjects.r(deltaInputValue)
    sourceReturn2 = robjects.r('''source('DrawROC.R')''')
    r_getname = robjects.globalenv['DrawROC']
    r_getname_delta = robjects.globalenv['Delta']
    sourceReturnValue = r_getname(r_getname_delta)
    print "  >> " + str(sourceReturnValue)
    return str(sourceReturnValue)

def setRWorkingDirectory():
    sourceReturn1 = robjects.r("path")
    return ""

@app.route('/todo/api/v1.0/DeltaSpecPpv', methods = ['GET'])
def deltaSpecPpv():
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
    shutil.copyfile("rplot.jpg","static/rplot.jpg")
    print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print returnString  
    print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    return jsonify({"returnstring" : returnString})
    #return returnString


if __name__ == '__main__':
    app.run(debug = True)

#if __name__ == '__main__':
#   "Are we in the __main__ scope? Start test server."
#    app.run(host='127.0.0.1',port=8080,debug=True)
