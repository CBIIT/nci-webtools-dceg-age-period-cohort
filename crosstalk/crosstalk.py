from flask import Flask, request
from rpy2.robjects import r
import traceback

app = Flask(__name__)
r.source('crosstalkWrapper.R')

@app.route('/calculate/', methods = ['POST'])
def calculate():
    try:
        return r.calculate(request.stream.read())[0]

    except Exception as e:
        print('------------EXCEPTION------------')
        traceback.print_exc(1)
        return str(e), 400

@app.after_request
def after_request(response):
    response.headers.add('Access-Control-Allow-Origin', '*')
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE')
    return response

if __name__ == '__main__':
    app.run('0.0.0.0', port = 10000, debug = True)

