from flask import Flask, request
from rpy2.robjects import r

app = Flask(__name__, static_folder='', static_url_path='')
r.source('apcWrapper.R')

@app.route('/calculate/', methods=['POST'], strict_slashes=False)
def calculate():
    return r.calculate(request.data.decode())[0]

@app.route('/apcRest/ping/', strict_slashes=False)
@app.route('/ping/', strict_slashes=False)
def ping():
    return r('"true"')[0]

@app.errorhandler(Exception)
def error_handler(e):
    """ Ensure errors are logged and returned """
    app.logger.error(str(e))
    return str(e), 400

@app.after_request
def after_request(response):
    response.headers.add('Access-Control-Allow-Origin', '*')
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE')
    return response

if __name__ == '__main__':
    app.run('0.0.0.0', port = 10000, debug = True, use_reloader = True)
