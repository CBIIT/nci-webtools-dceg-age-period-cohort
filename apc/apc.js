var apcModule = (function($) {
    var apcTool = {};

    /* Private variables used internally by the module */
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


    // ------------ EVENT HANDLERS ----------- //

    // ------ Read Dropped Text ------ //
    apcTool.dragTable = function(event) {
        // prevent page redirects
        event.preventDefault();

        // prevents event bubbling
        event.stopPropagation();

        // gets text that was dragged into this container and extracts non-empty lines
        var data = event.originalEvent.dataTransfer.getData("text/plain").match(/[^\r\n]+/g);

        // update model and redraw table
        apcModel.table = apcTool.createTable(data);
        apcTool.redrawTable();
        apcTool.resizePasteArea('#paste', '#inputTable');
    };

    // ------ Read Pasted Text ------ //
    apcTool.handlePaste = function(element) {
        element.on('paste', function(e) {
            setTimeout(function() {
                var data = element.val().match(/[^\r\n]+/g);
                element.val('');

                // update model and redraw table
                apcModel.table = apcTool.createTable(data);
                apcTool.redrawTable();
                apcTool.resizePasteArea('#paste', '#inputTable');
            }, 10);
        });
    };


    // ------ Read Uploaded File ------ //
    // Reads an uploaded file and updates the model with the contents
    apcTool.fileUpload = function() {

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

                apcTool.updateModel(contents);
                apcTool.redraw();
                apcTool.resizePasteArea('#paste', '#inputTable');
            };

            if (file)
                reader.readAsText(file);

        // Otherwise, attempt to use ActiveX
        } else {
            try {
                var filePath = $("#fileUpload").val();
                var fso = new ActiveXObject("Scripting.FileSystemObject");
                var textStream = fso.OpenTextFile(filePath);
                var fileData = textStream.ReadAll();
                console.log(fileData);
            } catch (e) {
                if (e.number == -2146827859) {
                alert('Unable to access local files due to browser security settings. ' +
                'To overcome this, go to Tools->Internet Options->Security->Custom Level. ' +
                'Find the setting for "Initialize and script ActiveX controls not marked as safe" and change it to "Enable" or "Prompt"');
                }
            }
        }
    };

    // ------ Initialize Download Results Selector ------ //
    apcTool.downloadResults = function() {
        var selected_file = $("#download_selector").val();
        window.open(selected_file, 'download');
        return false;
    };

    // ------------ UPDATE UI ------------ //

    // ------ Populate Input Fields ------ //
    apcTool.redraw = function() {
        $('#title').val(apcModel.title);
        $('#description').val(apcModel.description);
        $('#startYear').val(apcModel.startYear);
        $('#startAge').val(apcModel.startAge);
        $('#interval').val(apcModel.interval);

        if (apcModel.table !== null)
            apcTool.redrawTable();
    };

    // ------ Redraw Input Table ------ //
    apcTool.redrawTable = function() {
        if (apcModel.table === null) return;

        if (!apcTool.validate(apcModel.table)) {
            alert(paste_instructions);
        } else {
            var displayTable = [];

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
                ages = apcTool.createIntervals(startAge, interval, numRows);
                years = apcTool.createIntervals(startYear, interval, numCols / 2);

                // Update reference ages/years
                apcTool.updateReference(ages, years);
            } else {
                ages = [];
                for (var i = 0; i < numRows; i++) {
                    ages.push('');
                }
            }

            for (var j = 0; j < contents.length; j++) {
                displayTable.push([ages[j]]);
                for (var k = 0; k < contents[j].length; k++) {
                    var num = Number(contents[j][k]).toLocaleString().split('.')[0];
                    displayTable[j].push(num);

                    console.log('num is: ', num);
                }
            }

            $('#paste_here_image').hide();
            var tableID = apcTool.createInputTable('#tableContainer', apcTool.createHeaders(numCols), displayTable);

            $(tableID).addClass("nowrap cell-border ");
            headerRow = $(tableID).children().first();

            if (startYear && interval) {
                apcTool.createHeader(years).forEach(function(row) {
                    headerRow.prepend(row);
                });
            }
        }
    };


    apcTool.createInputTable = function(containerID, headers, data) {
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
        $('#tableContainer').find('.table').addClass('input-table');

        return tableID;
    };


    // ------------ HANDLE AJAX REQUESTS ------------ //

    // ------ Send Data To Server ------ //
    apcTool.sendRequest = function() {

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
            var descriptionText = 'Start Year: ' + $('#startYear').val() + ' at Age: ' + $('#startAge').val() + ' with Interval: ' + $('#interval').val()  + ' years';
            description.val(descriptionText);
        }

        $('.loading').css('display', 'block');

        $.post('/apcRest/', JSON.stringify(apcModel))
        .done(function(data) {
            apcTool.displayResults(data);
        })
        .always(function() {
            $('.loading').css('display', 'none');
        });
    };

    // ------ Display Results Data ------ //
    apcTool.displayResults = function(data) {
        var results = JSON.parse(data);
        console.log(results);

        graphKeys.forEach(function(key) {
            apcTool.loadImage(key, results[key]['pathToFile'][0]);
        });

        dataKeys.concat(graphKeys).forEach(function(key) {
            var table = results[key]['table'];

            for (var i = 0; i < table.length; i ++) {
                for (var k in table[i]) {
                    if (key == 'Waldtests') table[i][k] = apcTool.round(table[i][k], 4, 5);
                    else table[i][k] = apcTool.round(table[i][k], 3, 5);
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
    };

    // ------------ HELPER METHODS ----------- //

    // ------ Create row headers for input table ------ //
    apcTool.createHeaders = function(length) {
        var ageHeader = "Age";

        if (apcModel.table)
            ageHeader = '<span class = "ageGroups">' + apcModel.table.length + " age groups </span>";

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
    };

    // ------ Create Intervals ------ //
    // Creates and returns an array of ranges using the specified interval
    apcTool.createIntervals = function(initial, interval, length) {
        var intervals = [];

        for (var i = initial; i < initial + interval * length; i += interval) {
            var value = i;
            if (interval > 1) value += ' - ' + (i + interval - 1);
            intervals.push(value);
        }

        return intervals;
    };


    // ------ Create Table Matrix ------ //
    // Creates the input table from an array of lines
    apcTool.createTable = function(contents) {

        // splits each line by commas, spaces, or tabs
        return contents.map(function(line) {
            // converts each element to a floating point number
            return line.split(/[ \t,]+/).map(function(element) {
                return parseFloat(element);
            });
        });
    };

    // ------ Load Image ------ //
    // Populates the graphs
    apcTool.loadImage = function(keyData, pathToFile) {
        $('#' + keyData + 'Graph').html("<img style='min-width: 600px;' class='center-text' alt='graph for " + keyData + "' src= '" + pathToFile + "' />");
    };

    // ------ Parse Header ------ //
    // If this line contains a quoted description, extract its contents
    // otherwise, just split the line by commas and return the first value
    apcTool.parseHeader = function(line) {
        var description = line.match(/"(.*?)"/);

        if (description)
            line = description[1];
        else
            line = line.split(',')[0];

        // return the portion of this line after the first colon
        return line.split(/: (.+)?/, 2)[1].trim();
    };

    // ------ Reset Model ------ //
    apcTool.reset = function() {
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
        };

        // clear file upload
        $("#fileUpload").replaceWith($("#fileUpload").clone(true));
        $("#download_choice").hide();

        $('#tableContainer').empty();
        apcTool.createInputTable('#tableContainer', apcTool.createHeaders(6), apcTool.createMatrix(13, 10));
        $('#paste_here_image').show();

        // clear all tables and graphs
        dataKeys.concat(graphKeys).forEach(function(id) {
            $('#' + id).empty();
            $('#' + id + 'Graph').empty();
        });

        ['#ND', '#WT', '#CE'].forEach(function(key) {
            $(key).hide();
        });

        apcTool.redraw();
    };

    // ------ Update APC Model ------ //
    // Updates the apc model based on the contents of a CSV file
    // Parameters: contents - an array of lines in the file

    apcTool.updateModel = function(contents) {

        // the first five rows of the csv are the headers
        apcModel.title = apcTool.parseHeader(contents.shift());
        apcModel.description = apcTool.parseHeader(contents.shift());
        apcModel.startYear = parseInt(apcTool.parseHeader(contents.shift()));
        apcModel.startAge = parseInt(apcTool.parseHeader(contents.shift()));
        apcModel.interval = parseInt(apcTool.parseHeader(contents.shift()));
        apcModel.table = apcTool.createTable(contents);
    };

    apcTool.syncModel = function() {
        apcModel.title = $('#header').val();
        apcModel.description = $('#description').val();
        apcModel.startYear = $('#startYear').val();
        apcModel.startAge = $('#startAge').val();
        apcModel.interval = $('#interval').val();
    };

    apcTool.createHeader = function(years) {
        var title;

        if (apcModel.title !== '') {
            title = document.createElement('tr');

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
    };

    apcTool.validate = function(contents) {
        if (contents === null || !contents.length || contents.length < 2)
            return false;

        var length = contents[0].length;

        if (length % 2 !== 0 || length < 2)
            return false;

        contents.forEach(function(line) {
            if (line.length !== length) return false;
            line.forEach(function(element) {
                if (!parseFloat(element)) return false;
            });
        });

        return true;
    };

    // ------ Create Matrix  ------ //
    // Creates and returns a matrix with the specified number of rows and columns
    apcTool.createMatrix = function(rows, columns) {
        var matrix = [];

        for (var i = 0; i < rows; i++) {
            matrix[i] = apcTool.createArray(columns);
        }

        return matrix;
    };

    // ------ Create Array  ------ //
    // Creates and returns an array of the specified length containing zero width characters
    apcTool.createArray = function(length) {
        var array = [];

        for (var i = 0; i < length; i++) {
            array.push('&zwj;');
        }

        return array;
    };

    // ------ Round Floating Point Number (Specify length)------ //
    apcTool.round = function(x, digits, trim) {
        if (!parseFloat(x)) return x;
        return parseFloat(parseFloat(x.toFixed(digits)).toPrecision(trim));
    };


    /* ------ APC Functions ------ */
    // ------ Update contents of reference age/year selector ------ //
    apcTool.updateReference = function(ages, years) {

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
    };

    apcTool.updateCohortModel = function() {

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
    };

    apcTool.openHelpWindow = function(pageURL) {
        var helpWin = window.open(pageURL, "Help", "alwaysRaised,dependent,status,scrollbars,resizable,width=1000,height=800");
        helpWin.focus();
    };

    apcTool.toggleReference = function(id) {
        if (id == 'auto') {
            $('#referenceDiv').css("display", "none");
        }
        if (id == 'manual') {
            if ($.trim($('#startYear').val()) === '' || $.trim($('#startAge').val()) === '' || $.trim($('#interval').val()) === '') {
                alert("The value of start year, start age and interval must be selected.");
                $('#auto').prop("checked", true);
            } else {
                $('#referenceDiv').css("display", "block");
            }
        }
    };

    apcTool.resizePasteArea = function(textAreaID, tableID) {
        var textArea = $(textAreaID);

        textArea.width($('#tableContainer').innerWidth());
        textArea.height($(tableID).innerHeight());
    };

    return apcTool;
})($);

$(document).ready(function() {
    apcModule.createInputTable('#tableContainer', apcModule.createHeaders(6), apcModule.createMatrix(13, 10));

    $('#fileUpload').change(apcModule.fileUpload);
    $('#cancel').click(apcModule.reset);
    $('#calculate').click(apcModule.sendRequest);
    $("#download").click(apcModule.downloadResults);

    $('#title').change(apcModule.redrawTable);
    $('#description').change(apcModule.redrawTable);

    $('#interval').change(apcModule.redrawTable);

    $('#startYear').spinner({
        min: 1800,
        max: 2200,
        step: 1,
        spin: function(event, ui) {
            apcModule.redrawTable();
        },
        stop: function(event, ui) {
            apcModule.redrawTable();
        }
    });

    $('#refAuto, #refManual').on('change', function(e) {
        var refValue = $(e.target).val();
        e.stopPropagation();
        apcModule.toggleReference(refValue);
    });

    $('#help').on('click', function(e) {
        e.stopPropagation();
        apcModule.openHelpWindow('help.html');
    });

    $('#startAge').spinner({
        min: 0,
        max: 120,
        step: 1,
        spin: function(event, ui) {
            apcModule.redrawTable();
        },
        stop: function(event, ui) {
            apcModule.redrawTable();
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

    $('#refAge').change(apcModule.updateCohortModel);
    $('#refYear').change(apcModule.updateCohortModel);
    $('#apc-tab-nav').tabCollapse();

    var paste = $('#paste');
    paste.bind('drop', apcModule.dragTable);
    apcModule.handlePaste(paste);
    paste.click(function(e){
        e.preventDefault();
    });

    apcModule.resizePasteArea('#paste', '#inputTable');

    // allows elements to be dragged into this document
    document.addEventListener('dragover', function(event) {
        event.stopPropagation();
        event.preventDefault();
    });

    $(window).resize(function() {
        apcModule.resizePasteArea('#paste', '#inputTable');
    });
});
