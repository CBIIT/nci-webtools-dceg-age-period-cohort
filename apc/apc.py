from flask import Flask, request, send_file
from rpy2.robjects import r
import rpy2.robjects as ro
from rpy2.robjects import conversion

# Initialize rpy2 conversions for Flask threading
try:
    from rpy2.robjects import pandas2ri
    pandas2ri.activate()
except:
    pass

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

if __name__ == '__main__':
    @app.route('/')
    def index():
        return send_file('index.html')
    # Disable threading to avoid rpy2 context issues
    app.run('0.0.0.0', port = 10000, debug = False, use_reloader = False, threaded=False)
