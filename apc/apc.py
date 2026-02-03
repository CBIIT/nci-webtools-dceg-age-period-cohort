from flask import Flask, request, send_file, g
from rpy2.robjects import r
from rpy2.robjects import default_converter
from rpy2.robjects.conversion import localconverter

# Initialize rpy2 conversions
try:
    from rpy2.robjects import pandas2ri
    pandas2ri.activate()
except (ImportError, AttributeError):
    pass

app = Flask(__name__, static_folder='', static_url_path='')
app.config['APPLICATION_ROOT'] = '/apc'
r.source('apcWrapper.R')

@app.before_request
def setup_r_context():
    """Ensure rpy2 conversion context is available for this request"""
    g.r_converter = localconverter(default_converter)
    g.r_converter.__enter__()

@app.teardown_request
def cleanup_r_context(exception=None):
    """Clean up the converter context after request"""
    if hasattr(g, 'r_converter'):
        g.r_converter.__exit__(None, None, None)

@app.route('/')
@app.route('/index.html')
def index():
    return send_file('index.html')

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
    # Use single-threaded, multi-process mode to avoid rpy2 context issues
    # This matches the production mod_wsgi configuration (--processes 4 --threads 1)
    app.run('0.0.0.0', port = 10000, debug = False, use_reloader = False, threaded=False)
