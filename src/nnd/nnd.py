#!flask/bin/python
from flask import Flask, Response, abort, request, make_response, url_for, jsonify
from functools import wraps
from flask import current_app

import rpy2.robjects as robjects
from rpy2.robjects import r
import cgi
import shutil
import os
from xml.sax.saxutils import escape, unescape
from socket import gethostname
import json
import pandas as pd
import numpy as np
from pandas import DataFrame
import pandas.rpy.common as com
import urllib

app = Flask(__name__)

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


def setRWorkingDirectory():
    sourceReturn1 = robjects.r("path")
    return ""

def callFunctionWithPrev(parsedString):
    refSpec=urllib.unquote(parsedString[2].replace('=','<-'))
    print(refSpec)
    refSens=urllib.unquote(parsedString[3].replace('=','<-'))
    print(refSens)
    specArray=urllib.unquote(parsedString[4].replace('=','<-'))
    print(specArray)
    specArrayWithRef=urllib.unquote(parsedString[5].replace('=','<-'))
    print(specArrayWithRef)
    sensArray=urllib.unquote(parsedString[6].replace('=','<-'))
    print(sensArray)
    sensArrayWithRef=urllib.unquote(parsedString[7].replace('=','<-'))
    print(sensArrayWithRef)
    prev=urllib.unquote(parsedString[8].replace('=','<-'))
    print(prev)
    labels=urllib.unquote(parsedString[9].replace('=','<-'))
    print(labels)
    uniqueKey=urllib.unquote(parsedString[10].replace('=','<-'))
    print(uniqueKey)
    robjects.r(refSpec)
    robjects.r(refSens)
    robjects.r(specArray)
    robjects.r(specArrayWithRef)
    robjects.r(sensArray)
    robjects.r(sensArrayWithRef)
    robjects.r(prev)
    robjects.r(labels)
    robjects.r(uniqueKey)
    r_input_specref=robjects.globalenv[(refSpec.split("<-"))[0]]
    r_input_sensref=robjects.globalenv[(refSens.split("<-"))[0]]
    r_input_spec=robjects.globalenv[(specArray.split("<-"))[0]]
    r_input_specWithRef=robjects.globalenv[(specArrayWithRef.split("<-"))[0]]
    r_input_sens=robjects.globalenv[(sensArray.split("<-"))[0]]
    r_input_sensWithRef=robjects.globalenv[(sensArrayWithRef.split("<-"))[0]]
    r_input_prev=robjects.globalenv[(prev.split("<-"))[0]]
    r_input_labels=robjects.globalenv[(labels.split("<-"))[0]]
    r_input_key=robjects.globalenv[(uniqueKey.split("<-"))[0]]
    # source all R functions places in the director r-code
    rSource = robjects.r('source')
    rfilelist = []
    rfilelist += [each for each in os.listdir("./r-code") if each.endswith('.R')]
    for rfile in rfilelist:
        print " R files "+str(rfile)
 	rSource("./r-code/"+rfile)
 	rFunctionName=(rfile.split("."))[0]
 	r_getname_function = robjects.globalenv[str(rFunctionName)]
	if (rFunctionName == 'JsonWrapper'):
 	   jsonReturn = r_getname_function(r_input_sensWithRef, r_input_specWithRef, r_input_prev)
        elif (rFunctionName == 'DrawSensSpecLR'):
 	   r_getname_function(r_input_key, r_input_sensref, r_input_specref, r_input_sens, r_input_spec, r_input_labels)
    return jsonReturn[0]

def callFunction(parsedString):
    refSpec=urllib.unquote(parsedString[2].replace('=','<-'))
    print(refSpec)
    refSens=urllib.unquote(parsedString[3].replace('=','<-'))
    print(refSens)
    specArray=urllib.unquote(parsedString[4].replace('=','<-'))
    print(specArray)
    specArrayWithRef=urllib.unquote(parsedString[5].replace('=','<-'))
    print(specArrayWithRef)
    sensArray=urllib.unquote(parsedString[6].replace('=','<-'))
    print(sensArray)
    sensArrayWithRef=urllib.unquote(parsedString[7].replace('=','<-'))
    print(sensArrayWithRef)
    labels=urllib.unquote(parsedString[8].replace('=','<-'))
    print(labels)
    uniqueKey=urllib.unquote(parsedString[9].replace('=','<-'))
    print(uniqueKey)
    robjects.r(refSpec)
    robjects.r(refSens)
    robjects.r(specArray)
    robjects.r(specArrayWithRef)
    robjects.r(sensArray)
    robjects.r(sensArrayWithRef)
    robjects.r(labels)
    robjects.r(uniqueKey)
    r_input_specref=robjects.globalenv[(refSpec.split("<-"))[0]]
    r_input_sensref=robjects.globalenv[(refSens.split("<-"))[0]]
    r_input_spec=robjects.globalenv[(specArray.split("<-"))[0]]
    r_input_specWithRef=robjects.globalenv[(specArrayWithRef.split("<-"))[0]]
    r_input_sens=robjects.globalenv[(sensArray.split("<-"))[0]]
    r_input_sensWithRef=robjects.globalenv[(sensArrayWithRef.split("<-"))[0]]
    r_input_labels=robjects.globalenv[(labels.split("<-"))[0]]
    r_input_key=robjects.globalenv[(uniqueKey.split("<-"))[0]]
    # source all R functions places in the director r-code
    rSource = robjects.r('source')
    rfilelist = []
    rfilelist += [each for each in os.listdir("./r-code") if each.endswith('.R')]
    for rfile in rfilelist:
        print " R files "+str(rfile)
 	rSource("./r-code/"+rfile)
 	rFunctionName=(rfile.split("."))[0]
 	r_getname_function = robjects.globalenv[str(rFunctionName)]
	if (rFunctionName == 'JsonWrapperLR'):
 	   jsonReturn = r_getname_function(r_input_sensWithRef, r_input_specWithRef)
        elif (rFunctionName == 'DrawSensSpecLR'):
 	   r_getname_function(r_input_key, r_input_sensref, r_input_specref, r_input_sens, r_input_spec, r_input_labels)
    return jsonReturn[0]

@app.route('/nndRest/cal', methods = ['GET','POST'])
@jsonp
def biomarkerComparison():
    print " biomarkerComparison >> " + request.query_string + "\n";
    if request.method == 'GET':
       parsedString=request.query_string.split("&")
    elif request.method == 'POST':
       parsedString=request.stream.read().split("&")
    else:
       print "unhandled request type "+str(request.method)
    print(parsedString)
    # create R function input vectors
    # ignored the data at [0] and [7]. They are the jsonp
    # envelop artifacts which we dont care about.
    numberOfValues=(str(parsedString[1]).split('='))[1]
    print(numberOfValues)
    if (numberOfValues == "8"):
	jsonReturn = callFunctionWithPrev(parsedString)
        print jsonReturn
    	return jsonify({"returnstring" : jsonReturn})
    elif (numberOfValues == "7"):	
	jsonReturn = callFunction(parsedString)
        print jsonReturn
    	return jsonify({"returnstring" : jsonReturn})
    else:
    	return jsonify({"returnstring" : "1"})

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9982", help="Sets the Port") 
    # Default port is production value; prod,stage,dev = 9982, sandbox=9983
    args = parser.parse_args()
    port_num = int(args.port_number);
	
    hostname = gethostname()
    app.run(host='0.0.0.0', port=port_num, debug = True)
