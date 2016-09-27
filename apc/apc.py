from flask import Flask, request
from rpy2.robjects import r

app = Flask(__name__)
r.source('apcWrapper.R')

@app.route('/calculate/', methods = ['POST'])
def apc():
    return r.calculate(request.data)[0]
