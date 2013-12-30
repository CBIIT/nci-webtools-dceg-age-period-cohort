#!flask/bin/python
from flask import Flask, Response, abort, request, make_response, url_for, jsonify
from functools import wraps
from flask import current_app

import rpy2.robjects as robjects
import cgi, re
import shutil
import os
from xml.sax.saxutils import escape, unescape
import pandas as pd
import numpy as np
from pandas import DataFrame
import pandas.rpy.common as com
from socket import gethostname

#app = Flask(__name__)

# --- Brent Added...
app = Flask(__name__, static_folder='static', static_url_path='')

@app.route('/')
def root():
    return url_for('static', filename='index.html')
# --------

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


@app.route('/createPanCanList', methods = ['POST'])
@jsonp
def createPanCanList():
    urlinput = request.query_string
    print " url input  "+str(urlinput) 
    utlinput = request.query_string
    pancaninputstring = "pancaninput<-"+"\"?"+str(urlinput)+"\""
    print "pancaninputstring  >>"+str(pancaninputstring)
    robjects.r(str(pancaninputstring))
    robjects.r('''source('apcForRpy2.R')''')
    r_getname_pancaninput = robjects.globalenv['pancaninput']
    r_getname_getApcData = robjects.globalenv['getApcDataJSON']
    apcJsondata = r_getname_getApcData(r_getname_pancaninput )
    print "JSON Data "+str(apcJsondata)
    return jsonify({"returnstring" : str(apcJsondata[0])})

if __name__ == '__main__':
    hostname = gethostname()
    app.run(host='0.0.0.0', debug = True)
