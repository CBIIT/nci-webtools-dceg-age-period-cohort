from flask import Flask, request
import rpy2.robjects as wrapper

app = Flask(__name__)

wrapper.r['source']('rcode/CrossTalkWrapper.R')

@app.route('/crosstalkRest', methods = ['POST'])
@app.route('/crossTalkRest', methods = ['POST'])
@app.route('/crosstalkRest/', methods = ['POST'])
@app.route('/crossTalkRest/', methods = ['POST'])
def calculation():
    return wrapper.r['process'](request.stream.read())[0]

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # Default port is production value; prod, stage, dev = 8140, sandbox = 9140
    parser.add_argument('-p', dest = 'port_num', default='9140', help='Sets the Port') 
    parser.add_argument('--debug', action = 'store_true')
    args = parser.parse_args()

    app.run(host = '0.0.0.0', port = int(args.port_num), debug = args.debug, use_evalex = False) 
