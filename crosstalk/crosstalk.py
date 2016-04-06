from flask import Flask, request
import rpy2.robjects as robjects

app = Flask(__name__)
#robjects.r['source']('CrossTalkWrapper.R')

@app.route('/crossTalkRest/', methods = ['POST'])
def calculation():
    return '' 
    #robjects.r['getCrossTalkDataJSON'](request.stream.read())[0]

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9140", help="Sets the Port") 
    # Default port is production value; prod, stage, dev = 8140, sandbox = 9140
    args = parser.parse_args()
    port_num = int(args.port_number)

    app.run(host='0.0.0.0', port=port_num, debug=True, use_evalex=False) 
