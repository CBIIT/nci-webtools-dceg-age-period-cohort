# from flask import Flask, request
# app = Flask(__name__, static_folder='static', static_url_path='')

#@app.route('/')
#def root():
#   return url_for('static', filename='index.html')


from flask import Flask, render_template, Response, abort, request, make_response, url_for, jsonify
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
import urllib
from jinja2 import Environment, FileSystemLoader


#app = Flask(__name__)

# --- Brent Added...
app = Flask(__name__, static_folder='static', static_url_path='/static')

#@app.route('/')
#def root():
#    return url_for('static', filename='index.html')

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/glossary.html')
def glossary():
    return render_template('glossary.html')

@app.route('/help.html')
def help():
    return render_template('help.html')

@app.route('/index.html')
def root():
    return render_template('index.html')

def root1():
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


@app.route('/createPanCanList/list', methods = ['POST'])
@jsonp
def createPanCanList():
    robjects.r('''source('apcForRpy2.R')''')
    r_getname_getApcData = robjects.globalenv['getApcDataJSON']
    apcJsondata = r_getname_getApcData(request.stream.read())
    return apcJsondata[0]

def createStaticFiles():
    """ solves issue of copying templates to root index.html, help.html etc. Deleted 4 static files from github as there is no reason to check these files in."""
    env = Environment(loader=FileSystemLoader('templates'))
    files = os.listdir('./templates')
    for x in files:
        template = env.get_template(x)
        renderedFile = template.render(**{})
        f = open(x,'w')
        f.write(renderedFile)
        f.close()
        
if __name__ == '__main__':
    hostname = gethostname()
    createStaticFiles()
    app.run(host='0.0.0.0', port=8788, debug = True)


