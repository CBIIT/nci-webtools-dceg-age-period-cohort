from flask import Flask, request
from socket import gethostname
import rpy2.robjects as robjects

app = Flask(__name__)
robjects.r['source']('apcWrapper.R')

@app.route('/apcRest/', methods = ['POST'])
def apc():
    return robjects.r['getApcDataJSON'](request.stream.read())[0]

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9040", help="Sets the Port") 
    parser.add_argument("-d", dest="debug", default="False", help="Sets the Debugging Option") 
    # Default port is production value; prod, stage, dev = 8040, sandbox = 9040
    args = parser.parse_args()
    port_num = int(args.port_number)'
    
    if args.debug == 'True' :
    	debug = True
    else :
    	debug = False
    

    hostname = gethostname()
    app.run(host='0.0.0.0', port=port_num, debug = debug) 