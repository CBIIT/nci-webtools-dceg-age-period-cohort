from flask import Flask, request, jsonify
import rpy2.robjects as robjects
import json

app = Flask(__name__)

wrapper = robjects.r
wrapper['source']('crosstalkWrapper.R')

@app.route('/crosstalkRest', methods = ['POST'])
@app.route('/crosstalkRest/', methods = ['POST'])
def calculation():
    print(request.get_data())
    response = jsonify(data=json.loads(wrapper['process'](request.get_data())[0]), complete=True)
    return response

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
