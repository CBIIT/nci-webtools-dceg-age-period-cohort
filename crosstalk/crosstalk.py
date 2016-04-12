from flask import Flask, request, jsonify
import rpy2.robjects as robjects
import json

app = Flask(__name__)

wrapper = robjects.r
#wrapper['source']('rcode/test.R')

@app.route('/crosstalkRest', methods = ['POST'])
@app.route('/crosstalkRest/', methods = ['POST'])
def calculation():
    try:
        #wrapper['processClientJSON'](request.stream.read())[0]
        ex = example_file()
        print ex
        return jsonify(data=ex, complete=True)
    except Exception as e :
        return jsonify(data={}, error=e, complete=False)

def example_file():
    return json.loads(open('examples/example.json').read())

import argparse
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", dest="port_number", default="9140", help="Sets the Port")
    # Default port is production value; prod, stage, dev = 8140, sandbox = 9140
    args = parser.parse_args()
    port_num = int(args.port_number)

    app.run(host='0.0.0.0', port=port_num, debug=True) #, use_evalex=False)
