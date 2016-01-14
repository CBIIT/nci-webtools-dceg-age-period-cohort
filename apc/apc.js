var paste_instructions =
    "When pasting data from a spreadsheet or uploading\n" +
    "from a csv file, it should have alternating columns of\n" +
    "count-then-population for at least two age periods\n" +
    "(minimum size array = 2x2)";

var dataKeys = [
    "NetDrift",
    "Waldtests",
    "Coefficients"
];

var graphKeys = [
    "AgeDeviations",
    "PerDeviations",
    "CohDeviations",
    "LongAge",
    "CrossAge",
    "Long2CrossRR",
    "FittedTemporalTrends",
    "PeriodRR",
    "CohortRR",
    "LocalDrifts"
];

var apcModel = {
    title: '',
    description: '',
    startYear: null,
    startAge: null,
    interval: null,
    table: null,
    refYear: -1,
    refAge: -1,
    refCohort: -1
};

var firstRun = true;


$(document).ready(function() {

    createInputTable('#tableContainer', createHeaders(6), createMatrix(13, 10));

    $('#fileUpload').change(fileUpload);
    $('#cancel').click(reset);
    $('#calculate').click(sendRequest);
    $("#download").click(downloadResults);

    $('#title').change(redrawTable);
    $('#description').change(redrawTable);

    $('#interval').change(redrawTable);

    $('#startYear').spinner({
        min: 1800,
        max: 2200,
        step: 1,
        spin: function(event, ui) {
            redrawTable();
        },
        stop: function(event, ui) {
            redrawTable();
        }
    });

    $('#startAge').spinner({
        min: 0,
        max: 120,
        step: 1,
        spin: function(event, ui) {
            redrawTable();
        },
        stop: function(event, ui) {
            redrawTable();
        }
    });

    $("#please_wait").dialog({
        dialogClass: 'no-close',
        resizable: false,
        width: 'auto',

        autoOpen: false,
        hide: {
            effect: "fade",
            duration: 200
        }
    });

    $('#refAge').change(updateCohortModel);
    $('#refYear').change(updateCohortModel);

    $('#apc-tab-nav').tabCollapse();

    var paste = $('#paste');
    paste.bind('drop', dragTable);
    handlePaste(paste);
    paste.click(function(e){e.preventDefault();});

    resizePasteArea('#paste', '#inputTable');

    // allows elements to be dragged into this document
    document.addEventListener('dragover', function(event) {
        event.stopPropagation();
        event.preventDefault();
    });
});

$(window).resize(function() {
    resizePasteArea('#paste', '#inputTable');
});




// ------------ EVENT HANDLERS ----------- //

// ------ Read Dropped Text ------ //
function dragTable(event) {

    event.preventDefault();

    // prevent page redirects
    event.stopPropagation();

    // gets text that was dragged into this container and extracts non-empty lines
    var data = event.originalEvent.dataTransfer.getData("text/plain").match(/[^\r\n]+/g);

    // update model and redraw table
    apcModel.table = createTable(data);
    redrawTable();
    resizePasteArea('#paste', '#inputTable');
}


// ------ Read Pasted Text ------ //


function handlePaste(element) {
    element.bind('paste', function(e) {
        setTimeout(function() {
            var data = element.val().match(/[^\r\n]+/g);
            element.val('');

            // update model and redraw table
            apcModel.table = createTable(data);
            redrawTable();
            resizePasteArea('#paste', '#inputTable');
        }, 10);
    });
}


// ------ Read Uploaded File ------ //
// Reads an uploaded file and updates the model with the contents
function fileUpload() {

    // If this browser supports the Files API
    if (window.FileReader) {
        var file = this.files[0];
        var reader = new FileReader();

        reader.onload = function(event) {
            apcModel.refAge = -1;
            apcModel.refYear = -1;
            apcModel.cohort = -1;

            // split the file contents into an array of non-empty lines
            var contents = event.target.result.match(/[^\r\n]+/g);

            updateModel(contents);
            redraw();
            resizePasteArea('#paste', '#inputTable');
        };

        if (file) reader.readAsText(file);

    // Otherwise, attempt to use ActiveX
    } else {
        try {
            var filePath = $("#fileUpload").val();
            var fso = new ActiveXObject("Scripting.FileSystemObject");
            var textStream = fso.OpenTextFile(filePath);
            var fileData = textStream.ReadAll();
            console.log(fileData);

        }
        catch (e) {
            if (e.number == -2146827859) {
            alert('Unable to access local files due to browser security settings. ' +
            'To overcome this, go to Tools->Internet Options->Security->Custom Level. ' +
            'Find the setting for "Initialize and script ActiveX controls not marked as safe" and change it to "Enable" or "Prompt"');
            }
        }
    }


}

// ------ Initialize Download Results Selector ------ //
function downloadResults() {
    var selected_file = $("#download_selector").val();
    window.open(selected_file, 'download');
    return false;
}

// ------------ UPDATE UI ------------ //

// ------ Populate Input Fields ------ //
function redraw() {
    $('#title').val(apcModel.title);
    $('#description').val(apcModel.description);
    $('#startYear').val(apcModel.startYear);
    $('#startAge').val(apcModel.startAge);
    $('#interval').val(apcModel.interval);

    if (apcModel.table != null) redrawTable();
}

// ------ Redraw Input Table ------ //
function redrawTable() {

    if (apcModel.table == null) return;

    if (!validate(apcModel.table)) {
        alert(paste_instructions);
        this.preventDefault();
    } else {
        apcModel.title = $('#title').val();
        apcModel.description = $('#description').val();
        apcModel.startYear = parseFloat($('#startYear').val());
        apcModel.startAge = parseFloat($('#startAge').val());
        apcModel.interval = parseFloat($('#interval').val());


        // initialize the table
        contents = apcModel.table;

        numRows = contents.length;
        numCols = contents[0].length;

        // create the column headers
        startAge = parseFloat($('#startAge').val());
        startYear = parseFloat($('#startYear').val());
        interval = parseFloat($('#interval').val());

        // if the age, year and interval were specified
        // generate the age-interval column and the year-interval headers
        if (startAge && startYear && interval) {
            ages = createIntervals(startAge, interval, numRows);
            years = createIntervals(startYear, interval, numCols / 2);

            // Update reference ages/years
            updateReference(ages, years);
        } else {
            ages = [];
            for (var i = 0; i < numRows; i++) {
                ages.push('');
            }
        }

        var displayTable = [];

        for (var j = 0; j < contents.length; j++) {
            displayTable.push([ages[j]]);
            for (var k = 0; k < contents[j].length; k++) {
                displayTable[j].push(contents[j][k].toLocaleString());
            }
        }

        $('#paste_here_image').hide();
        var tableID = createInputTable('#tableContainer', createHeaders(numCols), displayTable);

        $(tableID).addClass("nowrap cell-border ");
        headerRow = $(tableID).children().first();

        if (startYear && interval) {
            createHeader(years).forEach(function(row) {
                headerRow.prepend(row);
            });
        }
    }
}


function createInputTable(containerID, headers, data) {

    var tableID = '#inputTable';
    var table = document.createElement('table');
    table.setAttribute('id', 'inputTable');
    table.setAttribute('class', 'table display compact');
    table.setAttribute('width', '100%');

    $(containerID).html(table);
    $(tableID).DataTable({
        "destroy": true,
        "data": data,
        "columns": headers,
        "bSort": false,
        "bFilter": false,
        "paging": false,
        "responsive": true,
        "dom": 't'
    });

    $('#tableContainer').find('#inputTable_wrapper').addClass('table-responsive');

    return tableID;
}


// ------------ HANDLE AJAX REQUESTS ------------ //

// ------ Send Data To Server ------ //
function sendRequest() {

    /* Populate title/description if empty */
    var title = $('#title');
    var description = $('#description');

    if (!title.val()) {
        var date = new Date();
        var titleText = 'APC Analysis - ' + date.getFullYear() + '_' +
                     date.getMonth() + 1 + '_' +
                     date.getDay() + '_' +
                     date.getHours() + '_' +
                     date.getMinutes();

        title.val(titleText);
    }

    if (!description.val()) {
        var descriptionText = 'Start Year: ' + $('#startYear').val() + ' at Age: ' + $('#startAge').val() + ' with Interval: ' + $('#interval').val()  + ' years' ;

        description.val(descriptionText);
    }

    $(".loading").css('display', 'block');

    $.post("/apcRest/", JSON.stringify(apcModel))
        .done(function(data) {
            displayResults(data);
        })
        .always(function() {
            $(".loading").css('display', 'none');
        });
}

// ------ Display Results Data ------ //
function displayResults(data) {

    var results = JSON.parse(data);
    console.log(results);

    graphKeys.forEach(function(key) {
        loadImage(key, results[key]['pathToFile'][0]);
    });


    dataKeys.concat(graphKeys).forEach(function(key) {

        table = results[key]['table'];

        for (var i = 0; i < table.length; i ++) {
            for (k in table[i]) {
                if (key == 'Waldtests') table[i][k] = round(table[i][k], 4, 5);
                else table[i][k] = round(table[i][k], 3, 5);
            }
        }


        keys = Object.keys(table[0])
        if ($.inArray('_row', keys) != -1) keys.unshift(keys.pop());

        headers = [];
        keys.forEach(function(key) {
            header = {
                "data": key,
                "title": key == '_row' ? '' : key
            };
            headers.push(header);
        });

        $('#' + key).DataTable({
            "destroy": true,
            "data": table,
            "columns": headers,
            "bFilter": false,
            "paging": false,
            "responsive": true,
            "aaSorting": [],
            "dom": 't'
        });

        $('#' + key).addClass("nowrap compact cell-border stripe hover");

    });


    if (firstRun) {
        $('#NetDrift').children().first().prepend('<tr role="row"><th colspan = "3" class="text-center"> Net Drift </th></tr>');
        $('#Waldtests').children().first().prepend('<tr role="row"><th colspan = "4" class="text-center"> Wald Tests </th></tr>');
        $('#Coefficients').children().first().prepend('<tr role="row"><th colspan = "5" class="text-center"> Coefficients </th></tr>');
        firstRun = false;
    }


    ['RDataInput', 'RDataOutput', 'TextInput', 'TextOutput', 'Excel'].forEach(function(key) {
        $('#' + key).attr('value', results[key]['pathToFile'][0]);
    });

    $("#download_choice").show();

    ['#ND', '#WT', '#CE'].forEach(function(key) {
        $(key).show();
    });
}



// ------------ HELPER METHODS ----------- //

// ------ Create row headers for input table ------ //
function createHeaders(length) {

    var ageHeader = "Age";
    if (apcModel.table) ageHeader = '<span class = "ageGroups">' + apcModel.table.length + " age groups </span>";

    var headers = [{
        title: ageHeader,
        className: 'dt-center grey'
    }];

    for (var i = 0; i < length; i += 2) {
        headers.push({
            title: "Count",
            className: 'dt-body-right'
        });
        headers.push({
            title: "Population",
            className: 'dt-body-right'
        });
    }

    return headers;
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


// ------ Create Table Matrix ------ //
// Creates the input table from an array of lines
function createTable(contents) {

    // splits each line by commas, spaces, or tabs
    return contents.map(function(line) {

        // converts each element to a floating point number
        return line.split(/[ \t,]+/).map(function(element) {
            return parseFloat(element);
        });
    });
}


// ------ Load Image ------ //
// Populates the graphs
function loadImage(keyData, pathToFile) {
    $('#' + keyData + 'Graph').html("<img style='min-width: 600px;' class='center-text' alt='graph for " + keyData + "' src= '" + pathToFile + "' />");
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


// ------ Reset Model ------ //
function reset() {

    // reset apc model
    apcModel = {
        title: '',
        description: '',
        startYear: null,
        startAge: null,
        table: null,
        interval: null,
        refYear: -1,
        refAge: -1,
        refCohort: -1
    }

    // clear file upload
    $("#fileUpload").replaceWith($("#fileUpload").clone(true));
    $("#download_choice").hide();

    $('#tableContainer').empty();
    createInputTable('#tableContainer', createHeaders(6), createMatrix(13, 10));
    $('#paste_here_image').show();

    // clear all tables and graphs
    dataKeys.concat(graphKeys).forEach(function(id) {
        $('#' + id).empty();
        $('#' + id + 'Graph').empty();
    });

    ['#ND', '#WT', '#CE'].forEach(function(key) {
        $(key).hide();
    });

    redraw();
}

// ------ Update APC Model ------ //
// Updates the apc model based on the contents of a CSV file
// Parameters: contents - an array of lines in the file

function updateModel(contents) {

    // the first five rows of the csv are the headers
    apcModel.title = parseHeader(contents.shift());
    apcModel.description = parseHeader(contents.shift());
    apcModel.startYear = parseInt(parseHeader(contents.shift()));
    apcModel.startAge = parseInt(parseHeader(contents.shift()));
    apcModel.interval = parseInt(parseHeader(contents.shift()));
    apcModel.table = createTable(contents);
}


function syncModel() {
    apcModel.title = $('#header').val();
    apcModel.description = $('#description').val();
    apcModel.startYear = $('#startYear').val();
    apcModel.startAge = $('#startAge').val();
    apcModel.interval = $('#interval').val();

}


function createHeader(years) {

    if (apcModel.title != '') {
        var title = document.createElement('tr');

        var emptyCell = document.createElement('th');
        emptyCell.setAttribute('class', 'white-border');
        title.appendChild(emptyCell);


        var titleCell = document.createElement('th');
        titleCell.setAttribute('class', 'header');
        titleCell.setAttribute('colspan', apcModel.table[0].length);

        var description = document.createElement('span');
        description.setAttribute('class', 'blue');
        description.innerHTML = apcModel.description;

        titleCell.innerHTML = apcModel.title;
        titleCell.appendChild(document.createElement('br'));
        titleCell.appendChild(description);

        title.appendChild(titleCell);
    }

    var row = document.createElement('tr');
    row.setAttribute('role', 'row');
    row.appendChild(document.createElement('th'));

    years.forEach(function(year) {
        var header = document.createElement('th');
        header.setAttribute('class', 'row-header');
        header.setAttribute('colspan', '2');
        header.innerHTML = year;
        row.appendChild(header);
    });

    return [row || '', title || ''];
}



function validate(contents) {
    if (contents == null || !contents.length || contents.length < 2)
        return false;

    var length = contents[0].length;

    if (length % 2 != 0 || length < 2)
        return false;

    contents.forEach(function(line) {
        if (line.length != length) return false;
        line.forEach(function(element) {
            if (!parseFloat(element)) return false;
        });
    });

    return true;
}


// ------ Create Matrix  ------ //
// Creates and returns a matrix with the specified number of rows and columns
function createMatrix(rows, columns) {

    var matrix = [];

    for (var i = 0; i < rows; i++) {
        matrix[i] = createArray(columns);
    }

    return matrix;
}

// ------ Create Array  ------ //
// Creates and returns an array of the specified length containing zero width characters
function createArray(length) {
    var array = [];

    for (var i = 0; i < length; i++) {
        array.push('&zwj;');
    }

    return array;
}

// ------ Round Floating Point Number (Specify length)------ //
function round(x, digits, trim) {

    if (!parseFloat(x)) return x
    return parseFloat(parseFloat(x.toFixed(digits)).toPrecision(trim))
}


/* ------ APC Functions ------ */


// ------ Update contents of reference age/year selector ------ //
function updateReference(ages, years) {

    $('#refAge').html("<option value='-1' selected>Age</option>");
    $('#refYear').html("<option value='-1' selected>Year</option>");

    ages.forEach(function(age) {
        var option = document.createElement('option');

        var values = age.toString().split('-');

        var refAge = values[0];
        if (values[1]) refAge = (parseFloat(values[0]) + parseFloat(values[1]) + 1.0) / 2;

        option.setAttribute('value', refAge.toString());
        option.innerHTML = age;
        $('#refAge').append(option);
    });

    years.forEach(function(year) {
        var option = document.createElement('option');

        var values = year.toString().split('-');
        var refYear = values[0];

        if (values[1]) refYear = (parseFloat(values[0]) + parseFloat(values[1]) + 1.0) / 2;

        option.setAttribute('value', refYear.toString());
        option.innerHTML = year;
        $('#refYear').append(option);
    });
}

function updateCohortModel() {

    year = parseFloat($('#refYear').val());
    age = parseFloat($('#refAge').val());

    if (year != -1 && age != -1) {
        cohort = year - age;

        apcModel.refAge = age;
        apcModel.refYear = year;
        apcModel.refCohort = cohort;

        $('#cohort').val(apcModel.refCohort.toString());
    } else {
        apcModel.refAge = -1;
        apcModel.refYear = -1;
        apcModel.refCohort = -1;

        $('#cohort').val('');
    }

}

function openHelpWindow(pageURL) {
    var helpWin = window.open(pageURL, "Help", "alwaysRaised,dependent,status,scrollbars,resizable,width=1000,height=800");
    helpWin.focus();
}

function toggleReference(id) {
    if (id == 'auto') {
        $('#referenceDiv').css("display", "none");
    }
    if (id == 'manual') {
        if ($.trim($('#startYear').val()) == '' || $.trim($('#startAge').val()) == '' || $.trim($('#interval').val()) == '') {
            alert("The value of start year, start age and interval must be selected.");
            $('#auto').prop("checked", true);
        } else {
            $('#referenceDiv').css("display", "block");
        }
    }
}

function resizePasteArea(textAreaID, tableID) {
    var textArea = $(textAreaID);

    textArea.width($('#tableContainer').innerWidth());
    textArea.height($(tableID).innerHeight());
}
