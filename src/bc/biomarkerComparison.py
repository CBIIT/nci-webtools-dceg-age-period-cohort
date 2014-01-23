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

@app.route('/BiomarkerComparison', methods = ['GET','POST'])
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
    refSpec=urllib.unquote(parsedString[1].replace('=','<-'))
    print(refSpec)
    refSens=urllib.unquote(parsedString[2].replace('=','<-'))
    print(refSens)
    specArray=urllib.unquote(parsedString[3].replace('=','<-'))
    print(specArray)
    sensArray=urllib.unquote(parsedString[4].replace('=','<-'))
    print(sensArray)
    prevArray=urllib.unquote(parsedString[5].replace('=','<-'))
    print(prevArray)
    uniqueKey=urllib.unquote(parsedString[6].replace('=','<-'))
    print(uniqueKey)
    robjects.r(refSpec)
    robjects.r(refSens)
    robjects.r(specArray)
    robjects.r(sensArray)
    robjects.r(prevArray)
    robjects.r(uniqueKey)
    r_input_specref=robjects.globalenv[(refSpec.split("<-"))[0]]
    r_input_sensref=robjects.globalenv[(refSens.split("<-"))[0]]
    r_input_spec=robjects.globalenv[(specArray.split("<-"))[0]]
    r_input_sens=robjects.globalenv[(sensArray.split("<-"))[0]]
    r_input_prev=robjects.globalenv[(prevArray.split("<-"))[0]]
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
 	   jsonReturn = r_getname_function(r_input_sens, r_input_spec, r_input_prev)
        elif (rFunctionName == 'DrawSensSpecLR'):
 	   r_getname_function(r_input_key, r_input_sensref, r_input_specref, r_input_sens, r_input_spec)
    print jsonReturn[0]
    return jsonify({"returnstring" : jsonReturn[0]})



if __name__ == '__main__':
    hostname = gethostname()
    app.run(host='0.0.0.0', port=8989,debug = True)
