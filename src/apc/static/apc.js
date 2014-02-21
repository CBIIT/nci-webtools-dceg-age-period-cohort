var error_reason;
var paste_instructions = 
	"When pasting from a spreadsheet or uploading from a csv file,\n it should have alternating " +
	"columns of count-then-population\n" +
	"for at least two age periods (minimum size array = 2x2),\n no more than 100 age groups and periods.";
var line_array; // This is global.  Not set until a paste or upload
var csvResultData = {};

var open_threads; // Used to determine if all calls have returned
var error_count = 0;


$(document).ready(function() { 

	var main_table = $("#inputData").html();
	
	$("#paste").css("width",  $("#main-table").innerWidth() - 10);
	$("#paste").css("height",  $("#main-table").innerHeight() - 10);
	
	var off = $("#paste_here_image").position();
//	alert (off.left + "," + off.top);

	
	$( "#R_upload_button" ).click(function() {
		alert("Coming Soon");
	});
	$( "#csv_upload_button" ).click(function() {
		alert("Coming Soon");
	});

//	$("#please_wait").dialog({
//		dialogClass: "no-titlebar",
//	    height: '140px',
//	});
	
	$("#please_wait").dialog({
		dialogClass: 'no-close',
		resizable: false,
	    width: '140px',
	    autoOpen: false,
	    hide: { effect: "fade", duration: 200 }
	});
	

	create_paste_binding($(".paste_area"));

	$( "#cancel" ).click(function() { 
/*
			$(".data").empty();
			$("#main-table").empty();
			$("#inputData").html(main_table);
			$("#startYear").val("");
			$("#startAge").val("");
			$("#description").val("");
			$("#interval").val("");
			$("#title").val("");
			$("#Excel").html("");
			$("#InputRawData").html("");
			$("#OutputRawData").html("");
			$("#ND").css("display","none");
        	$("#CE").css("display","none");
        	$("#WT").css("display","none");
        	$(".rates").css("display","none");
        	$("#paste_here_image").css("display","none");
			$("#countPopulation").val("");
            $("#paste_here_image").show();
            set_paste_area_size();
            
        	for (var x=8; x < 18; x++) {
        		items = $.merge($("#tab-4458-"+x).find("table"),$("#tab-4458-"+x).find("div").not("[class='rates']").not("[class='dataTables_wrapper']"))
        		for (var z=0; z < items.length; z++)
	        		{
	        			$("#"+items[z].id).html("")
	        		}
        	}

        	// tabTablesDivs = ['AgeDeviationsGraph','AgeDeviations','AgeDeviationsDownloadResultLink','PerDeviationsGraph','PerDeviations','CohDeviationsGraph','CohDeviations','LongAgeGraph','LongAge','CrossAgeGraph','CrossAge','Long2CrossRRGraph','Long2CrossRR','FittedTemporalTrendsGraph','FittedTemporalTrends','PeriodRRGraph','PeriodRR','CohortRRGraph','CohortRR','LocalDriftsGraph','LocalDrifts']
        	// for (var x=0; x<tabTablesDivs.length;x++)
	        // 	{
	        // 		$("#"+tabTablesDivs[x]).html("");
	        // 	}

			line_array = null;

//            $("#cancel").css("display","none");
			$("#download_choice").hide();


//	    create_paste_binding($(".paste_area"));
*/
		window.location.reload(true);
		return true;
	});
	
	
	$( "#calculate" ).click(function() {

    	if ($("#title").val()=='') {
    		
//			var date_str = new Date().getTime();
    		var d = new Date();
			var date_str = "" 
				+ d.getFullYear() + "_" 
				+ (d.getMonth() + 1) + "_" // Why is january = 0?
				+ d.getDate() + "_"
				+ d.getHours() + "_"
				+ d.getMinutes();
    		
    		$("#title").val('APC Analysis - ' + date_str)
    		setTableTitle();
    	}
    	else {
			var title = encodeURIComponent($("#title").val());
    	}
    	
    	var title = encodeURIComponent($("#title").val());
    	var startYear = $("#startYear").val();
     	var startAge = $("#startAge").val();
     	var interval = $("#interval").val();

     	if ($("#description").val()=='') {
     		var description = "Start Year: " 
     			+ startYear + " at Age: " 
     			+ startAge + " with Interval: " 
     			+ getInterval() + " years";
     			$("#description").val(description);
     			setTableTitle();
     	} else {
     		var description = encodeURIComponent($("#description").val());
     	}

     	if (line_array == undefined || line_array == null || line_array.length == 0) {
    		alert(paste_instructions);
    		return false;
    	}


    	if (isNaN(startYear) || startYear < 1900 || startYear > 2200) {
    		alert("Start year must be a valid number beween 1900 and 2200");
    		return false;
    	}
    	if (isNaN(startAge) || startAge < 1 || startAge > 120) {
    		alert("Start year must be a valid number beween 0 and 120");
    		return false;
    	}
    	if (interval != "year1" && interval != "year2" &&
    		interval != "year3" && interval != "year4" && 
    		interval != "year5" && interval != "year6" &&
    		interval != "year7" && interval != "year8" && 
    		interval != "year9" && interval != "year10") {
    		alert("Interval must be 1,2,3,4,5,6,7,8,9 or 10 years, it is: [" + interval + "]");
    		return false;
    	}
    	
      	var periodCount= numberOfPeriods();
     	var rowCount = numberOfRows();
     	//var countCSV = encodeURIComponent(determine_count_array());
     	var countCSV = determine_count_array();
     	//var populationCSV = encodeURIComponent(determine_pop_array());
     	var populationCSV = determine_pop_array();
     	var uniqueId = generateUID ();
     
		var keys = ["AgeDeviations","PerDeviations","CohDeviations", 		"LongAge", 
					"CrossAge", 	"Long2CrossRR", "FittedTemporalTrends",	"PeriodRR",
					"CohortRR",		"LocalDrifts",	"Coefficients",			"Waldtests", 
					"NetDrift",		"Offset",		"RawData", "Excel" ];
		//-----
        $("#please_wait").dialog('open');
        $("#ND").css("display","block");
        
        $("#CE").css("display","block");
        $("#WT").css("display","block");
        $(".rates").css("display","block");

        open_threads = keys.length;
		for (var i in keys){
			getAPCData({
		                	key: keys[i],
		                	title: title, 
		                	startYear: startYear, 
		                	startAge:  startAge, 
		                	interval:  interval, 
		                	periodCount: periodCount, 
		                	rowCount: rowCount, 
		                	countCSV: countCSV, 
		                	populationCSV: populationCSV,
		                	uniqueId: uniqueId,
		                	description: description
		                }, keys[i], uniqueId);
			
		}
		return false;

	});
    
	$( "#startAge" ).change(function() {
		on_change_starting_age();		
	});

	$( "#title" ).change(function() {
		//display_table(text);
		setTableTitle();
	});

	$( "#description" ).change(function() {
		//display_table(text);
		setTableTitle();
	});

	$( "#startYear" ).change(function() {
		on_change_start_year();
	});

	$( "#interval" ).change(function() {
		on_change_starting_age();
		on_change_start_year();
	});

	$( "#loadExampleData" ).click(function() {
		// TODO: send data to Larry's code somehow
	});
	
    $('#countPopulation').change(function(){
                if ( window.FileReader ) {
                        var file = this.files[0];
                        if ($("#title").val()=='') {
                        	var filename = file.name;
                        	filename = filename.substr(filename.lastIndexOf('/')+1)
                        	var new_title = filename.substr(0, filename.lastIndexOf('.'));
                        	$("#title").val(new_title);
                        }
                        var reader = new FileReader();
                        reader.onload = function(theFile) {
	                        var text =reader.result;
	                        display_table(text, ',');
                        };

                        reader.readAsText(file);
                }
                else {
                        if ($("#title").val()=='') { 
                        	var filename = $("#countPopulation").val();
                        	filename = filename.substr(filename.lastIndexOf('/')+1)
                        	var new_title = filename.substr(0, filename.lastIndexOf('.'));
                        	$("#title").val(new_title);
                        }
                        var filePath = $("#countPopulation").val();
                        var fso = new ActiveXObject("Scripting.FileSystemObject");
                        var textStream = fso.OpenTextFile(filePath);
                        var fileData = textStream.ReadAll();
                        display_table(fileData,',');
                }
                $("#cancel").css("display","block");
                $("#paste_here_image").hide();
        
    });
    
    $("#download").click(function() {
    	var selected_file = $("#download_selector").val();
    	window.open("./" + selected_file, 'download');
//    	alert ("File is: " + selected_file);
    	return false;
    });

});
function setTableTitle () {
	$("#table-title").html($("#title").val()+ " <br/> <span style='color:blue'>" + $("#description").val() + "</span>");
}

function generateUID() {
    return ("000000" + (Math.random()*Math.pow(36,6) << 0).toString(36)).substr(-6)
}

function getAPCData(data, keyData, uniqueId){
//	var start_time = new Date().getTime();
//	console.log("Starting Ajax Call [" + data.key + "] at [" + start_time + "]");
        hostname = window.location.hostname;
	$.ajax({
			type: 'POST',
			//type: 'GET',
			url: "/createPanCanList/list",
			//url: "http://analysistools-dev.nci.nih.gov/createPanCanList/list",
			//url: "http://"+hostname+"/createPanCanList/list",
			data: data,
			success: function(data) {
				if (keyData.localeCompare("Offset") != 0 &&
					keyData.localeCompare("Excel") != 0 &&
					keyData.localeCompare("RawData") != 0 ) {
					fillDataTable(data, keyData);
				} else {
					displayText(data, keyData);
				}
				if (keyData.localeCompare("Waldtests") != 0 && 
					keyData.localeCompare("Coefficients") != 0 && 
					keyData.localeCompare("NetDrift") != 0 &&
					keyData.localeCompare("Offset") != 0 &&
					keyData.localeCompare("Excel") != 0 &&
					keyData.localeCompare("RawData") != 0 ){
					loadImage(keyData, uniqueId);	
				}

			},
			error: function(request, textStatus, errorThrown) {
				error_count++;
				console.log("ERROR: " + request);
			},
			complete: function(data) {
				console.log("Completing: " + keyData);
				open_threads--;
//				var end_time = new Date().getTime();
//				console.log("Ending Ajax Call [" + keyData + "] at [" + end_time + "][" + open_threads + "]");
				if (open_threads == 0) {
					$("#please_wait").dialog('close');
					if (error_count > 0) {
						alert ("There were " + error_count + " errors with your request");
						error_count=0;
					}
				}
				
				if (keyData == "NetDrift" && $("#netdrift_header").length == 0) $("#NetDrift").find("thead").prepend(
					"<tr><th id='netdrift_header' colspan='4' style='text-align:center;'>Net Drift </th></tr>");
				if (keyData == "Waldtests" && $("#waldtest_header").length == 0) $("#Waldtests").find("thead").prepend(
					"<tr><th id='waldtest_header' colspan='4' style='text-align:center;'>Wald Tests </th></tr>");
				if (keyData == "Coefficients" && $("#coefficients_header").length == 0) $("#Coefficients").find("thead").prepend(
					"<tr><th id='coefficients_header' colspan='5' style='text-align:center;'>Coefficients </th></tr>");

				
			},
			dataType: "json"
			//dataType: "jsonp"
		});
}
function displayText(jsonData, key){
	if (key.localeCompare("Offset") == 0) {
		$('#OffsetLongAge').text(jsonData[0]);
		$('#OffsetCrossAge').text(jsonData[0]);
		$('#OffsetFittedTemporalTrends').text(jsonData[0]);	
	} else if (key.localeCompare("RawData") == 0) {
		$("#download_choice").show();
		
		$("#raw_input_option").attr("value", jsonData[0]);
		$("#raw_output_option").attr("value", jsonData[1]);
		$("#text_input_option").attr("value", jsonData[2]);
		$("#text_output_option").attr("value", jsonData[3]);
		
	} else {
		$("#excel_output_option").attr("value", jsonData[0]);
	}
	
}

function fillDataTable(jsonTableData, key){
	var columnHeaderData2d = getColumnHeaderData (jsonTableData, key);
	$('#' + key).dataTable( {
		"aaData": jsonTableData,
		"aoColumns": columnHeaderData2d,
		"bAutoWidth" : false,
		"bFilter": false,
		"bSearchable":false,
		"bInfo":false,
		"bPaginate": false,
		"bDestroy": true
	});		
}

// function createResultDownloadLink (keyData, uniqueId){
// 	$('#' + keyData + 'DownloadResultLink').html("<a href='csv/" + keyData + uniqueId + ".csv'>Download " + keyData +" CSV Data</a>");
// }

function loadImage(keyData, uniqueId) {
	$('#' + keyData + 'Graph').html("<img style='width: 600px ; height: 480px' class='center' alt='graph for " 
			+ keyData + "' src='./static/img/" + keyData + uniqueId + ".png'>");
}

function getColumnHeaderData(data2d, keyName) {
	var columnHeaderData2d = new Array();
	
	for (var key in data2d[0]){
		var tempObject = {};
		tempObject["mDataProp"] = key;
		tempObject["sTitle"] = key;
		columnHeaderData2d.push(tempObject);
	}
	return columnHeaderData2d;
}

function JSON2CSV(objArray) {
    //var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;

    var str = '';
    var line = '';

    for (var index in array[0]) {
    	line += index + ',';
    }

    line = line.slice(0, -1);
    str += line + '\r\n';

    for (var i = 0; i < array.length; i++) {
        var line = '';

    	for (var index in array[i]) {
    		line += array[i][index] + ',';
    	}
    }

    line = line.slice(0, -1);
    str += line + '\r\n';
    return str;    
}

 
function upload_file() {
//	var file = document.getElementById('files').files[0];
	var files_element = document.getElementById('files');
	var files = files_element.files;
	
	if (!files|| !files.length) {
		alert('Please select a file!');
		return;
	}

	var file = files[0];
	var reader = new FileReader();

	// If we use onloadend, we need to check the readyState.
	reader.onload = function(evt) {
		if (evt.target.readyState == FileReader.DONE) { // DONE == 2
			parse_file(evt.target.result);
		}
	};

	var blob = file.slice(0, file.size);
	reader.readAsBinaryString(blob);
}

function parse_file(blob) {
//	var text = blob.replace(/,/g,'\t');	
//	display_table(text, '\t');
	display_table(text, ",");
}

function create_paste_binding (element) {
    element.bind('paste', function(e)  {
        var self = this;
        $(this).val("");
        setTimeout(function(e) {
        	var txt = $(self).val();
        	var status = display_table(txt, '\t');
            if (status) {
            	$("#cancel").css("display","block");
            	$("#paste_area").val("Input Pasted");
            }
            else $("#paste_area").val("Input Failed \nneeds to be 14,9");
        }, 0); 
        $("#paste_here_image").hide();
     });	 
}

function on_change_starting_age() {
	if (line_array == null) return;
	
	if ($("#startAge").val() == "" ) return;
	
	var starting_age = parseInt($("#startAge").val());
	
	var num_ages = line_array.length - 1;
	for (var y=0;y<num_ages;y++) {
		$("#age_" + y).html(compute_age(y));
	}
}

function getInterval (){
	var intervalString = $("#interval").val();
	var interval = 1; // 1 is the default
	switch (intervalString) {
		case "year1":
			interval = 1;
		break;
		case "year2":
			interval = 2;
		break;
		case "year3":
			interval = 3;
		break;
		case "year4":
			interval = 4;
		break;
		case "year5":
			interval = 5;
		break;
		case "year6":
			interval = 6;
		break;
		case "year7":
			interval = 7;
		break;
		case "year8":
			interval = 8;
		break;				
		case "year9":
			interval = 9;
		break;
		case "year10":
			interval = 10;
		break;		
	}
	return interval;
}

function on_change_start_year() {
	if (line_array == null) return;
	
	var first_period = parseInt($("#startYear").val());
	
	var num_periods = Math.floor((line_array[0].length + 1) / 2);
	for (var x=0;x<num_periods;x++) {
		var period_start = compute_period(x);
		var period_end = period_start + getInterval() - 1;

		// When start and end are both is same year, just display it once
		var period_display;
		if (period_start == period_end) period_display = period_start;
		else period_display = period_start +  " - " + period_end;

		$("#period_" + x).html(period_display);
	}
}

function determine_count_array() {
	
	var num_periods = numberOfPeriods();
	var num_ages = numberOfRows();
	
	var counts = "";
	for (var y=0;y<num_ages;y++) {
		for (var x=0;x<num_periods;x++) {
			counts += line_array[y][(x*2)] + ',';
		}
	}
	return counts;
}

function determine_pop_array() {

	var num_periods = numberOfPeriods();
	var num_ages = numberOfRows();
	
	var pop_array = "";
	for (var y=0;y<num_ages;y++) {
		for (var x=0;x<num_periods;x++) {
			pop_array += line_array[y][((x*2)+1)] + ',';
		}
	}
	return pop_array;	
}

function numberOfPeriods () {
	return Math.floor((line_array[0].length + 1) / 2);
}

function numberOfRows () {
	return line_array.length - 1;
}
// function determine_combined_array() {

// 	var num_periods = Math.floor((line_array[0].length + 1) / 2);
// 	var num_ages = line_array.length - 1;
	
// 	var combo_str = ""; // Must set this to empty otherwise it will add "udefined" to [0][0] cell
// 	for (var x=0;x<num_periods;x++) {
// 		for (var y=0;y<num_ages;y++) {
// 			combo_str += line_array[(y*2)][x] + "," + line_array[(y*2)+1][x];
// 		}
// 		combo_str +="\n";
// 	}
// 	return combo_str;	
// }

function display_table(txt, delimiter) {
	
	
	
	var lines = txt.split("\n");
	line_array = new Array(lines.length);
	for (count = 0; count < lines.length;count++) {
		//line_array[count] = lines[count].split("\t");
		line_array[count] = lines[count].split(delimiter);
	}

	var test_another_row = true;
	var title = null;
	var description = null;
	var start_year = null;
	var start_age = null;
	var year = null;
	for (var i=0; test_another_row; i++) {
		test_another_row = false;
		if (line_array[i] == null) break;
		var first_cell_in_row = "" + line_array[i][0];
		if (first_cell_in_row.substring(0, 5) == "Title") {
			title = first_cell_in_row.substring(7);  // Everything after Title:_
			test_another_row = true;
		} else if (first_cell_in_row.substring(0, 11) == "Description") {
			description = first_cell_in_row.substring(13);  // Everything after Description:_
			test_another_row = true;
		} else if (first_cell_in_row.substring(0, 12) == "\"Description") {
			description = first_cell_in_row.substring(14);  // Everything after Description:_
			test_another_row = true;
		} else if (first_cell_in_row.substring(0, 10) == "Start Year") {
			start_year = first_cell_in_row.substring(12);  // Everything after Start Year:_
			test_another_row = true;
		} else if (first_cell_in_row.substring(0, 9) == "Start Age") {
			start_age = first_cell_in_row.substring(11);  // Everything after Start Age:_
			test_another_row = true;
		} else if (first_cell_in_row.substring(0, 16) == "Interval (Years)") {
			year = first_cell_in_row.substring(18);  // Everything after Interval (Year):_
			test_another_row = true;
		} else if (first_cell_in_row == "" || isNaN(first_cell_in_row)) {
			test_another_row = true; // Also remove blank lines			
		}
	}
	var metadata_lines = i-1; // i will be 1 higher than the number of metadata lines
	
	if (metadata_lines > 0) {
		for (count = metadata_lines; count < lines.length;count++) {
			line_array[count - metadata_lines] = line_array[count];
		}
		line_array = line_array.splice(0, line_array.length-metadata_lines);

		if (title != null) $("#title").val(title);
		if (description != null)$("#description").val(description);
		if (start_year != null)$("#startYear").val(start_year);	
		if (start_age != null)$("#startAge").val(start_age);	
		if (year != null)$("#interval").val("year" + year);	
	}
	
	// Check for all conditions in which data size is wrong
	if (!validate_paste_input(line_array)) {
		alert(error_reason + "\n\n" + paste_instructions);
		return false;
	}
	
	var rows;
	
	// Sometimes the data has a blank row at the end, do not iterate on it.
	if (line_array[line_array.length -1][0] == "") rows = line_array.length;
	else rows = line_array.length + 1;
	
	// Note each 'col' is actually 2 columns in the data and tables
	var cols = Math.floor((line_array[0].length + 1) / 2);
		
	// We are going to completely replace this table with a new one of the appropriate size

	$("#main-table").empty();
	
	
	var third_header_row = $("<tr></tr>");
	third_header_row.append("<td >&nbsp;</td>");
	third_header_row.append("<td class='header border-left border-top'>&nbsp;</td>");
	var table_title = $("<td id='table-title' colspan='" + (cols * 2) + "' class='header border-bottom border-right border-left  border-top'>" + $("#title").val() + "<br/> <span style='color:blue'>" + $("#description").val()  + "</span></td>");

	third_header_row.append(table_title);
	
	$("#main-table").append(third_header_row);
	
	var fourth_header_row = $("<tr></tr>");
	fourth_header_row.append("<td></td>");
	fourth_header_row.append("<td class='header border-left border-right'></td>");
	for (var x =0; x < cols; x++) {
// 		var cell_left = $("<td class='data border-dotted-left' id='Period_" + x + "_count'>" + compute_period(x) + "</th>");
//		fourth_header_row.append(cell_left);
//		var cell_right = $("<td class='data' id='Period_" + x + "_pop'>" + compute_period(x) + "</th>");
//		fourth_header_row.append(cell_right);
//		if (x == cols - 1) cell_right.addClass("border-right");
		var period_start = compute_period(x);
		var period_end = period_start + getInterval() - 1;
		
		// When start and end are both is same year, just display it once
		var period_display;
		if (isNaN(period_start) || isNaN(period_end) ) period_display = ""; 
		else if (period_start == period_end) period_display = period_start;
		else period_display = period_start +  " - " + period_end;

		var cell = $("<td colspan='2' class='header border-dotted-left' id='period_" + x + "'>" + 
				period_display +  "</th>");
		fourth_header_row.append(cell);
		if (x == cols - 1) cell.addClass("border-right");

	}
	$("#main-table").append(fourth_header_row);

	var fifth_header_row = $("<tr></tr>");
	fifth_header_row.append("<td></td>");
	fifth_header_row.append("<td class='header border-bottom border-left border-right'>Age</th>");
	for (var x =0; x < cols; x++) {
		var cell_left = $("<td class='header border-bottom border-dotted-left'>Count</th>");
		fifth_header_row.append(cell_left);
		var cell_right = $("<td class='header border-bottom'>Population</th>");
		fifth_header_row.append(cell_right);
		if (x == cols - 1) cell_right.addClass("border-right");
	}
	$("#main-table").append(fifth_header_row);

	// We have to recalculate the # of rows in case some were metadata and removed
	if (line_array[line_array.length -1][0] == "") rows = line_array.length - 1 ;
	else rows = line_array.length;

	// Note each 'col' is actually 2 columns in the data and tables
	cols = Math.floor((line_array[0].length + 1) / 2);

	////  Main Data Element
	for (var y=0; y < rows; y++) {
		var data_row = $("<tr></tr>");
		// First Column is label, because of rowspan, only first row has it
		if (y == 0) data_row.append("<td id='row-title' rowspan='" + rows + "' " +
				"class='row-header border-top border-bottom border-left'>" +
				"<div class='vertical-text'>Age<br/><i>(" + rows + "&nbsp;Age&nbsp;Groups)</i></div></td>")
		
		var age = compute_age(y);
		if (isNaN(age)) age = "";
		var age_cell = $("<td id='age_" + y + "' class='header border-left border-right'>" + age + "</td>");
				
		if (y == rows - 1) age_cell.addClass("border-bottom");
		data_row.append(age_cell);
		for (var x=0; x<cols;x++) {
			var count_value = line_array[y][2*x];
			var population_value = line_array[y][2*x+1];
			
			var cell = $("<td  class='data border-dotted-left'> " + addCommas(count_value) + "</td>");
			cell.attr("id", "D_" + y + "_" + x + "_count");
			if (y == rows-1 ) cell.addClass("border-bottom");
			
			data_row.append(cell);
			var cell = $("<td class='data'> " + addCommas(population_value) + "</td>");
			if (y == rows-1 ) cell.addClass("border-bottom");
			if (x == cols-1 ) cell.addClass("border-right");
			cell.attr("id", "D_" + y + "_" + x + "_pop");
			
			data_row.append(cell);
		}
		$("#main-table").append(data_row);
	}
	
	set_paste_area_size();
//	create_paste_binding(paste_area_box);
	
	return true;
}

function set_paste_area_size() {
	// Ok, now we built the whole table.  We need to find out exactly where and how big the data area is for the 
	// Invisible paste-box

	var total_width = $("#main-table").innerWidth();
	if (total_width > 600) total_width = 600; // Can't be larger than window
	var total_height = $("#main-table").innerHeight() - 20;
		
	var offset = $("#main-table").offset();
	
	$("#paste")
	var paste_area_box = $("#paste");
	paste_area_box.css("height", total_height);
	paste_area_box.css("width", total_width);
	paste_area_box.css("left", offset.left);
	paste_area_box.css("top", offset.top);	
}

function compute_age (num) {
	var starting_age = parseInt($("#startAge").val());
	var interval = getInterval();//parseInt($("#interval").val());
	
//	alert ("["+starting_age+"]["+interval+"]")
	return starting_age + (num * interval);
}

function compute_period(num) {
	var first_period = parseInt($("#startYear").val());
	var interval = getInterval();//parseInt($("#interval").val());
	
	return first_period + (num * interval);	
}

// error_reason is a global that provides insight as to why the paste did not work.
function validate_paste_input (line_array) {
	var rows = line_array.length;
	var cols = line_array[0].length;

	if (rows == 0 || cols == 0) { error_reason = "Data is Empty"; return false; }
	if ( (cols % 2) != 0) { error_reason = "Not an even number of columns"; return false; } 
	if (rows < 2)  { error_reason = "Less than 2 rows"; return false; }
	if (cols < 2)  { error_reason = "Less than 2 columns"; return false; }
	if (rows > 100)  { error_reason = "Too many rows (more than 100)"; return false; }
	if (cols > 200)  { error_reason = "Too many columns (more than 200)"; return false; }
	
	// check that all numbers are really numbers
	var rows = line_array.length;
	// Note each 'col' is actually 2 columns in the data and tables
	var cols = Math.floor((line_array[0].length + 1) / 2);

	var all_numbers = true;
	var bad_values = "";
	var num_errors=0;
	for (var y=0; y < rows-1; y++) {
		for (var x=0; x<cols;x++) {

			// Scrub the numbers in the array for any characters INCLUDING commas
			var before_clean;
			before_clean = line_array[y][2*x];
			line_array[y][2*x] = before_clean.replace(/[^\d\.\-\ ]/g, '');
			before_clean = line_array[y][2*x+1];
			line_array[y][2*x+1] = before_clean.replace(/[^\d\.\-\ ]/g, '');
			
			var count_value = line_array[y][2*x];
			var population_value = line_array[y][2*x+1];
			
			if (isNaN(count_value)) { 
				all_numbers=false; 
				bad_values += "[" + count_value + "]"; 
				num_errors++;
			}
			if (isNaN(population_value)) { 
				all_numbers=false; 
				bad_values += "["+population_value+"]"; 
				num_errors++;
			}
			// No reason to show too many errors
			if (num_errors >= 5) { error_reason = "At least 5 non-numbers found: " + bad_values; return false; }
	
		}	
	}
	if (!all_numbers) { error_reason = "Some non-numbers found: " + bad_values; return false; }
	
	
	return true;
}

// Number formatter
function addCommas(nStr)
{
	nStr += '';
	x = nStr.split('.');
	x1 = x[0];
	x2 = x.length > 1 ? '.' + x[1] : '';
	var rgx = /(\d+)(\d{3})/;
	while (rgx.test(x1)) {
		x1 = x1.replace(rgx, '$1' + ',' + '$2');
	}
	return x1 + x2;
}

function openHelpWindow(pageURL) {
                window.open(pageURL, "Help", "alwaysRaised,dependent,status,scrollbars,resizable,width=1000,height=800");
}
