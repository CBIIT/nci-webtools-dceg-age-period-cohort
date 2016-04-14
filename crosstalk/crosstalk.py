from flask import Flask, request, jsonify
import rpy2.robjects as robjects
import json

app = Flask(__name__)

wrapper = robjects.r
wrapper['source']('crosstalkWrapper.R',chdir=True)

@app.route('/crosstalkRest', methods = ['POST'])
@app.route('/crosstalkRest/', methods = ['POST'])
def calculation():
    print(request.get_data())
    response = jsonify(data=json.loads(wrapper['process'](request.get_data())[0]), complete=True)
    return response

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9140", help="Sets the Port")
    # Default port is production value; prod, stage, dev = 8140, sandbox = 9140
    args = parser.parse_args()
    port_num = int(args.port_number)

    app.run(host='0.0.0.0', port=port_num, debug=True) #, use_evalex=False)
