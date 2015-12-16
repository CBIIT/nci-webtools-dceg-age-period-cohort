var keys = ["AgeDeviations", "PerDeviations", "CohDeviations", "LongAge",
    "CrossAge", "Long2CrossRR", "FittedTemporalTrends", "PeriodRR",
    "CohortRR", "LocalDrifts", "Coefficients", "Waldtests",
    "NetDrift", "Offset", "RawData", "Excel"
];

// This is the apc data model that will be sent to the server
var apcModel = {
    title: '',
    description: '',
    startYear: 0,
    startAge: 0,
    interval: 0,
    count: [[]],
    population: [[]],
    refYear: -1,
    refAge: -1,
    refCohort: -1
}

$(document).ready(function() {
    $('#fileUpload').change(fileUpload);
});

// ------ Handle File Upload ------ //

function fileUpload() {
    var file = this.files[0];
    var reader = new FileReader();

    reader.onload = function(event) {
        var contents = event.target.result.split("\n");
        updateModel(contents);
        console.log(apcModel);
        $('#results').html("Created model. See the console for output.")
    }

    reader.readAsText(file);
}

// ------ Handle Dropped Text ------ //

// TODO: parses text that is dropped into the table and updates the model
function dropTable(e) {
    var text = e.originalEvent.dataTransfer.getData('Text');
}

// ------------ Helper Functions ------------ //

// ------ Update APC Model ------ //
// Updates the apc model based on the contents of a CSV file
// Parameter: contents - an array of lines in the file

function updateModel(contents) {

    // the first five rows of the csv are the headers
    apcModel.title = parseHeader(contents.shift());
    apcModel.description = parseHeader(contents.shift());
    apcModel.startYear = parseInt(parseHeader(contents.shift()));
    apcModel.startAge = parseInt(parseHeader(contents.shift()));
    apcModel.interval = parseInt(parseHeader(contents.shift()));

    // split the rest of the lines into a matrix
    for (var i = 0; i < contents.length; i++) {

        // if not empty, split the line into an array
        if (contents[i].length) {
            contents[i] = contents[i].split(',');
        }

        // remove empty lines
        else {
            contents.splice(i, 1);
            i--;
        }
    }

    updateModelTables(contents);
}

// ------ Update APC Model Tables ------ //
// Updates the apc model's count and population tables

function updateModelTables(contents) {

    // initialize the tables
    apcModel.count = createMatrix(contents.length);
    apcModel.population = createMatrix(contents.length);

    // populate the apc model's count and population fields
    for (var j = 0; j < contents.length; j++) {
        for (var k = 0; k < contents[j].length / 2; k++) {
            apcModel.count[j].push(parseFloat(contents[j][2 * k]));
            apcModel.population[j].push(parseFloat(contents[j][1 + 2 * k]));
        }
    }
}

// ------ Parse Dropped Text ------ //
// Converts dropped text into an array that can be passed into updateModelTablse

function parseDroppedText(contents) {

}

// ------ Create Matrix  ------ //
// Creates and returns a matrix with the specified number of rows

function createMatrix(rows) {
    var matrix = [];

    for (var i = 0; i < rows; i++) {
        matrix[i] = [];
    }

    return matrix;
}

// ------ Parse Header ------ //
// If this line contains a quoted description, extract its contents
// otherwise, just split the line by commas and return the first value

function parseHeader(line) {
    var description = line.match(/"(.*?)"/);

    if (description) line = description[1];
    else line = line.split(',')[0];

    // return the portion of this line after the first colon
    return line.split(/: (.+)?/, 2)[1].trim();
}