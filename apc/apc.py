from flask import Flask, request
import rpy2.robjects as robjects

app = Flask(__name__)
robjects.r['source']('apcWrapper.R')

@app.route('/apcRest/', methods = ['POST'])
def apc():
    return robjects.r['getApcDataJSON'](request.stream.read())[0]
