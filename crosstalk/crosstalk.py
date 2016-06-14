from flask import Flask, request, jsonify
import json
import os
import rpy2
import sys

app = Flask(__name__)

wrapper = rpy2.robjects.r
wrapper['source']('CrossTalkWrapper.R')
def buildFailure(data):
  response = jsonify(data=data, complete=False)
  response.mimetype = 'application/json'
  response.status_code = 400
  return response

def buildSuccess(data):
  response = jsonify(data=data, complete=True)
  response.mimetype = 'application/json'
  response.status_code = 200
  return response

@app.route('/crosstalkRest', methods = ['POST'])
@app.route('/crosstalkRest/', methods = ['POST'])
def calculation():
    try:
      print(request.get_data())
      response = buildSuccess(json.loads(wrapper['process'](request.get_data())[0]))
    except Exception as e:
      exc_type, exc_obj, exc_tb = sys.exc_info()
      fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
      print("EXCEPTION------------------------------", exc_type, fname, exc_tb.tb_lineno)
      response = buildFailure(str(e))
    return response


# Specify either calculate, fitModel, or generateExcel
@app.route('/crosstalkRest/<action>', methods = ['POST'])
def process():
    try:
      print(request.get_data())

      if action in ['calculate', 'fitModel', 'generateExcel']:
        return buildSuccess(json.loads(wrapper[action](request.get_data())[0]))
    
    except Exception as e:
      exc_type, exc_obj, exc_tb = sys.exc_info()
      fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
      print("EXCEPTION------------------------------", exc_type, fname, exc_tb.tb_lineno)
      return buildFailure(str(e))


@app.after_request
def after_request(response):
    response.headers.add('Access-Control-Allow-Origin', '*')
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE')
    return response

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    # Default port is production value; prod, stage, dev = 8140, sandbox = 9140
    parser.add_argument('-p', dest = 'port_num', default='9140', help='Sets the Port')
    parser.add_argument('--debug', action = 'store_true')
    args = parser.parse_args()
    app.run(host = '0.0.0.0', port = int(args.port_num), debug = args.debug, use_evalex = False)
