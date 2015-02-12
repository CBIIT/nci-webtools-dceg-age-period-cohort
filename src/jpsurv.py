#!flask/bin/python
from flask import Flask, render_template, Response, abort, request, make_response, url_for, jsonify, redirect
from functools import wraps
from flask import current_app

import cgi
import shutil
#!flask/bin/python
from flask import Flask, render_template, Response, abort, request, make_response, url_for, jsonify, redirect
from functools import wraps
from flask import current_app

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
#from LDpair import calculate_pair
#from LDproxy import calculate_proxy
#from LDmatrix import calculate_matrix
#from LDhap import calculate_hap

#import os
#from flask import Flask, request, redirect, url_for
from werkzeug import secure_filename

tmp_dir = "./tmp/"

app = Flask(__name__, static_folder='', static_url_path='/')
#app.debug = True

#app = Flask(__name__, static_folder='static', static_url_path='/static')

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
    data =[{'Age groups': ['0-49','50-65s','65+'], 'Breast stage': ['Localized','Regional','Distant'],'Test group': ['val1','ValTwo','AnotherValue']}]
    out_json = json.dumps(data)

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

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9982", help="Sets the Port")
    # Default port is production value; prod,stage,dev = 9982, sandbox=9983
    args = parser.parse_args()
    port_num = int(args.port_number);

    hostname = gethostname()
    app.run(host='0.0.0.0', port=port_num, debug = True)

