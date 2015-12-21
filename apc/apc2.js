var paste_instructions = 
	"When pasting from a spreadsheet or uploading from a csv file,\n it should have alternating " +
	"columns of count-then-population\n" +
	"for at least two age periods (minimum size array = 2x2),\n no more than 100 age groups and periods.";

var dataKeys =  [   "NetDrift", 
                    "Waldtests", 
                    "Coefficients"  ];

var graphKeys = [   "AgeDeviations",
                    "PerDeviations",
                    "CohDeviations",
                    "LongAge",
                    "CrossAge",
                    "Long2CrossRR",
                    "FittedTemporalTrends",
                    "PeriodRR",
                    "CohortRR",
                    "LocalDrifts"   ];
var apcModel = {};

$(document).ready(function() {
    
    reset();
    
    $('#fileUpload').change(fileUpload);
    $('#cancel').click(reset);
    $('#calculate').click(sendRequest);
    initializeInputTable('#tableContainer', createHeaders(6), createMatrix(11, 10));
    
	$('#startYear').spinner({
		min: 0,
		max: 10000,
		step: 1,
		spin: function(event, ui){updateInputTable()},
		stop: function(event, ui){updateInputTable()}
	});
    
	$('#startAge').spinner({
		min: 0,
		max: 120,
		step: 1,
		spin: function(event, ui){updateInputTable()},
		stop: function(event, ui){updateInputTable()}
	});
    
    $('#interval').change(updateInputTable);
//    $('#refYear').change(calculateCohort);
//    $('#refAge').change(calculateCohort);
    
    
    $("#please_wait").dialog({
		dialogClass: 'no-close',
		resizable: false,
	    width: '141x',
	    autoOpen: false,
	    hide: { effect: "fade", duration: 200 }
	});
    
	$('#inputData').bind('drop', function (e) {
		// gets text that was dragged into this container and extracts non-empty lines
		var data = e.originalEvent.dataTransfer.getData("text/plain").match(/[^\r\n]+/g);
            
        updateTable(data);
	});
    
    
    $("#download").click(function() {
    	var selected_file = $("#download_selector").val();
    	window.open(selected_file, 'download');
    	return false;
    });

        
    // allows elements to be dragged into this document
    document.addEventListener('dragover', function(event) {
        event.preventDefault();
    });
});


// ------------ DOM Manipulation ------------ //

function redraw() {
    $('#title').val(apcModel.title);
    $('#description').val(apcModel.description);
    $('#startYear').val(apcModel.startYear);
    $('#startAge').val(apcModel.startAge);
    $('#interval').val(apcModel.interval);
    
    
    
}





function reset() {
    
    // reset apc model
    apcModel = {
        title: '',
        description: '',
        startYear: null,
        startAge: null,
        interval: null,
        count: [[]],
        population: [[]],
        refYear: -1,
        refAge: -1,
        refCohort: -1
    }
    
    // clear file upload
    $("#fileUpload").replaceWith($("#fileUpload").clone(true));
    $("#download_choice").hide();

    
    // clear all divs
    dataKeys.concat(graphKeys).forEach(function(id) {
        $('#' + id).empty();
    });
    
    redraw();
}


// ------ Update Model Tables ------ //
// Inputs: Unparsed array of lines

// Updates the APC model's tables and displays them
function updateTable(contents) {
	
    // splits each line by commas, spaces, or tabs
    contents = contents.map(function(line) {
		return line.split(/[ \t,]+/);
    });
    
    if (!validate(contents)) {
        alert("invalid input");
    }
    
    else {
        
        // initialize the tables
        apcModel.count = createMatrix(contents.length, 0);
        apcModel.population = createMatrix(contents.length, 0);
        
        // populate the apc model's count and population fields
        for (var j = 0; j < contents.length; j++) {
            for (var k = 0; k < contents[j].length / 2; k++) {
                apcModel.count[j].push(parseFloat(contents[j][2 * k]));
                apcModel.population[j].push(parseFloat(contents[j][1 + 2 * k]));
            }
        }
        
        // create the column headers
        startAge = apcModel.startAge || parseFloat($('#startAge').val());
        startYear = apcModel.startYear || parseFloat($('#startYear').val());
        interval = apcModel.interval || parseFloat($('#interval').val());
        
        numRows = apcModel.count.length;
        numCols = apcModel.count[0].length;
        
        // if the age, year and interval were specified
        // generate the age-interval column and the year-interval headers
        if (startAge != null && startYear != null && interval != null) {
            ages = createIntervals(startAge, interval, numRows);
            years = createIntervals(startYear, interval, numCols/2);
            
            console.log('ages',ages);
            console.log('years', years);
        } else {
            ages = createArray(numRows);
        }
        
        // create the display matrix
        displayTable = [];
        
        // set the ages column
        ages.forEach(function(age) {
            displayTable.push([age.toLocaleString()]);
        });
        
        
        // update count and population fields
        for (var j = 0; j < contents.length; j ++) {
            for (var k = 0; k < contents[j].length; k ++) {
                displayTable[j].push(parseFloat(contents[j][k]).toLocaleString());
            }
        }
        
        console.log(displayTable);
        $('#paste_here_image').zIndex(-1);
        var table = initializeInputTable('#tableContainer', createHeaders(numCols), displayTable);
        
        headerRow = $('#inputTable').children().first();
        
        headerRow.prepend(createYearIntervalsHeaders(years));
//            '<tr role="row"><th colspan = "' + displayTable[0].length + '" > Test Row </th></tr>'
//        );
        
        console.log(headerRow);
        redraw();
    }
}




// ------ Update Input Table ------ //

function updateInputTable() {
//     updateTable(apcModel.contents);
}



// ------------ HELPER METHODS -----------

// ------ Send Data To Server ------ //

function sendRequest() {
    
    $("#please_wait").dialog('open');
    
    $.post("/apcRest/", JSON.stringify(apcModel))
        .done(function(data) {
            console.log(data);
            displayResults(data);
        })
        .always(function() {
            $("#please_wait").dialog('close');
        });
}



// ------ Read Uploaded File ------ //
// Reads an uploaded file and updates the model with the contents
function fileUpload() {
    var file = this.files[0];
    var reader = new FileReader();

    reader.onload = function(event) {
        
        // split the file contents into an array of non-empty lines
        var contents = event.target.result.match(/[^\r\n]+/g);
        updateModel(contents);
        console.log(apcModel);
    }

    reader.readAsText(file);
}

// ------ Update APC Model ------ //
// Updates the apc model based on the contents of a CSV file
// Parameter: contents - an array of lines in the file

function updateModel(contents) {

    // the first five rows of the csv are the headers
    apcModel.title          = parseHeader(contents.shift());
    apcModel.description    = parseHeader(contents.shift());
    apcModel.startYear      = parseInt(parseHeader(contents.shift()));
    apcModel.startAge       = parseInt(parseHeader(contents.shift()));
    apcModel.interval       = parseInt(parseHeader(contents.shift()));

    updateTable(contents);
}


// ------ Create Intervals ------ //
// Creates and returns an array of ranges using the specified interval
function createIntervals(initial, interval, length) {
    var intervals = [];
    
    for (var i = initial; i < initial + interval * length; i += interval) {
        
        var value = i;
        if (interval > 1) value += ' - ' + (i + interval - 1);
        intervals.push(value);
    }
    
    return intervals;
}


// ------ Create Matrix  ------ //
// Creates and returns a matrix with the specified number of rows and columns
function createMatrix(rows, columns) {
	
	var matrix = [];
    
	for (var i = 0; i < rows; i ++) {
		matrix[i] = createArray(columns);
	}
	
	return matrix;
}

// ------ Create Array  ------ //
// Creates and returns an array of the specified length containing zero width characters
function createArray(length) {
    var array = [];
    
    for (var i = 0; i < length; i ++) {
        array.push('&zwj;');
    }
    
    return array;
}

// ------ Create row headers for input table ------ //
function createHeaders(length) {
	var headers = [{title: "Age"}];
	
	for (var i = 0; i < length; i += 2) {
        headers.push({title: "Count"});
        headers.push({title: "Population"});
	}
	
	return headers;
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


function initializeInputTable(containerID, headers, data) {
	
	var table = document.createElement('table');
	table.setAttribute('id', 'inputTable');
	table.setAttribute('class', 'display');
	table.setAttribute('width', '100%');
	
	$(containerID).html(table);
	$('#inputTable').DataTable({
		"destroy": true,
		"data": data,
		"columns": headers,
		"bSort": false,
		"bFilter": false,
		"paging": false,
		"responsive": true,
		"dom": 't'
	})
    
    return table;
}

function createYearIntervalsHeaders(years) {
    
'<tr role="row"><th colspan = "' + displayTable[0].length + '" > Test Row </th></tr>'    
    var row = '<tr role="row">';
    
    row += '<th></th>'
    years.forEach(function (year) {
        row += '<th colspan = "2">' + year + '</th>';
    });
    
    
    return row + '</tr>'
}


// ------ Create Table ------ //
// Creates a DataTable using the specified matrix



function createTable(contents) {
}


function validate(contents) {
    if (!contents.length)
        return false;
    
    var length = contents[0].length;
    
    if (length % 2 != 0)
        return false;
    
    contents.forEach(function (line) {
        if (line.length != length) return false
    });
    
    return true;
}


function displayResults(data) {
    
    var results = JSON.parse(data);
    
    graphKeys.forEach(function(key) {
        loadImage(key, results[key]['pathToFile'][0]);
    });
    
    graphKeys.forEach(function(key) {
        
        table = results[key]['table'];
        
        headers = [];
        Object.keys(table[0]).forEach(function(key) {
           header = {
               "data": key,
               "title": key
               
           };
           headers.push(header); 
        });
        
        
        
        console.log(table, headers);
        
        $('#' + key).DataTable({
            "destroy": true,
            "data": table,
            "columns": headers,
            "bSort": false,
            "bFilter": false,
            "paging": false,
            "responsive": true,
            "dom": 't'
        });
        
        
        
        
    });
    
    ['RDataInput', 'RDataOutput', 'TextInput', 'TextOutput', 'Excel'].forEach(function (key) {
        $('#' + key).attr('value', results[key]['pathToFile'][0]);
    });
    
    
    $("#download_choice").show();
    console.log(results);
    
}


function loadImage(keyData, pathToFile) {
	$('#' + keyData + 'Graph').html("<img style='width: 600px ; height: 480px' class='center' alt='graph for "
			+ keyData + "' src= '" + pathToFile + "' />");
}