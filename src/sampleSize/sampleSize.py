# We need to import the jsonify object, it will let us
# output json, and it will take care of the right string
# data conversion, the headers for the response, etc
import os
from flask import Flask, render_template, request, jsonify
from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage
from rpy2.robjects.vectors import IntVector, FloatVector

with open ('sampleSizeWrapper.R') as fh:
        rcode = os.linesep.join(fh.readlines())
        wrapper = SignatureTranslatedAnonymousPackage(rcode,"wrapper")

# Initialize the Flask application
app = Flask(__name__)

@app.route('/')
def index():
    # Render template
    return render_template('index.html')

# This route will return a list in JSON format
@app.route('/sampleSizeRest/', methods=['POST'])
def sampleSizeRest():
    # Get the parsed contents of the form data
    data = request.json
    #print(json)

    k=data["k"].split(',')
    prev=data["prev"]
    N=data["N"]
    unique_id=data["unique_id"]
    fixed_flag=data["fixed_flag"]

    if fixed_flag == "Specificity":
	sens=data["sens"].split(',')
        spec=data["spec"]
        tabs=spec.split(",")
        print "calling saveSensContours"
	for tab in tabs:
		wrapper.saveSensContours(IntVector(k), FloatVector(sens), float(tab), float(prev), IntVector(N), unique_id, tab)
    else:
        spec=data["spec"].split(',')
        sens=data["sens"]
	tabs=sens.split(",")
        print "calling saveSpecContours"
	for tab in tabs:
 		wrapper.saveSpecContours(IntVector(k), float(tab), FloatVector(spec), float(prev), IntVector(N), unique_id, tab)


    #for tab in tabs:
    	#wrapper.saveSensContours(IntVector(k), FloatVector(sens), float(tab), float(prev), IntVector(N), unique_id, tab)

#saveSensContours 
#saveSpecContours

    return jsonify(data)

if __name__ == '__main__':
    app.debug = True
    app.run(
        host="0.0.0.0",
        port=int("9120")
    )
