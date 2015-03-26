var control_data;
var jpsurvData = {"file":{"dictionary":"Breast.dic","data":"something.txt", "form":"form-983832.json"}, "calculate":{"form": {"yearOfDiagnosisRange":[]}, "static":{}}, "plot":{"form": {}, "static":{"imageId":0} }, "tokenId":"unknown", "status":"unknown"};

//var yearOfDiagnosisVarName = "Year_of_diagnosis_1975";
//var year_of_diagnosis_title = "Year fo Diagnosis 1975+";
//var years;
if(getUrlParameter('tokenId')) {
	jpsurvData.tokenId = getUrlParameter('tokenId');
}
if(getUrlParameter('status')) {
	jpsurvData.status = getUrlParameter('status');
}

//var years = [
//	                                   "1975","1976","1977","1978","1979",
//	"1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
//	"1990","1991","1982","1983","1984","1985","1986","1987","1988","1989",
//	"2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
//	"2010","2011"
//];
//Constant series of events.   On a daily events.
//var cohort_covariance_variable_names
var cohort_covariance_variables;
/*
var cohort_covariance_variables = {
	"Age groups": ["0-49","50-65s","65+"],
	"Breast stage": ["Localized","Regional","Distant"],
	"Test group": ["val1","ValTwo","AnotherValue"]
}
*/

$(document).ready(function() {

	console.log("jpsurvData");
	console.dir(jpsurvData);

	var status = getUrlParameter('status');
	if(status == "uploaded") {
		$('#file_control_container')
				.empty()
				.append($('<div>')
					.append('Dictionary File: ')
					.append($('<span>')
							.append(jpTrim(getUrlParameter('file_control_filename'), 30))
							.attr('title', getUrlParameter('file_control_filename'))
						)
					);
		$('#file_data_container')
				.empty()
				.append($('<div>')
					.append('Data File: ')
					.append($('<span>')
							.append(jpTrim(getUrlParameter('file_data_filename'), 30))
							.attr('title', getUrlParameter('file_data_filename'))
						)
					);

		$('#upload_file_submit_container').remove();
		//var file_control_output = load_ajax(getUrlParameter('file_control_filename'));
		//console.log(file_control_output	);

		//var file_data_output = load_ajax(getUrlParameter('file_data_filename'));

		setUploadData();
		console.log(jpsurvData.file.form);
		var output_file = load_ajax("form-" + jpsurvData.tokenId + ".json");
		control_data = output_file;

		console.log("Output file...")
		console.dir(control_data);

//		alert('Updating R Output');

/*
		$( "<a>" )
			.append('Reset')
			.attr('href', '/jpsurv')
			.css('padding-left', '25px')
			.insertAfter('#calculate');
*/
		load_form();
		/*
		$( "<hr>" ).insertBefore('#file_control_output');

		$('#file_control_output').empty()
			.append($('<h2>').append('File Control Output').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(JSON.stringify(file_control_output)));

		$('#file_data_output').empty()
			.append($('<h2>').append('File Data Output').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(jpTrim(JSON.stringify(file_data_output), 1000)));

		$('#r_output').empty()
			.append($('<h2>').append('R Output File').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(JSON.stringify(output_file)));
		*/
	} else {
			$("#parameter_submit").on("click", build_output_format_column);
	}

	$("#cohort_select").on("change", change_cohort_select);
	$("#covariate_select").on("change", change_covariate_select);
	$("#upload_file_submit").on("click", file_submit);
	$("#calculate").on("click", setCalculateData);
	$("#plot").on("click", setPlotData);
	//$("#calculate").on("click", show_graph_temp);

});

function setUploadData() {
	//Set Stage 1 upload data to jpsurvData
	//Set file data
	jpsurvData.file.dictionary = getUrlParameter('file_control_filename');
	jpsurvData.file.data = getUrlParameter('file_data_filename');
	jpsurvData.file.form = getUrlParameter('output_filename');
	jpsurvData.status = getUrlParameter('status');

	console.log("setUploadData()");
	console.dir(jpsurvData);

}

function setCalculateData() {
	//Set static data
	var inputAnswers;
	// = $('#parameters').serialize();
  //var yearOfDiagnosisVarName="Year_of_diagnosis_1975";  //HARD CODED...Why?
  //Remove + from title
  var yearOfDiagnosisVarName = jpsurvData.calculate.static.yearOfDiagnosisTitle.replace('+', '');
  yearOfDiagnosisVarName = yearOfDiagnosisVarName.replace(new RegExp(" ", 'g'), "_");

  //Remove spaces and replace with underscore
	jpsurvData.calculate.static.yearOfDiagnosisVarName = yearOfDiagnosisVarName;
	jpsurvData.calculate.static.seerFilePrefix = jpsurvData.file.dictionary.substring(0, jpsurvData.file.dictionary.indexOf("."));

	jpsurvData.calculate.static.allVars = get_cohort_covariance_variable_names();
	jpsurvData.calculate.static.allVars.push(yearOfDiagnosisVarName);

  //dynamic form data
  // cohort
	jpsurvData.calculate.form.cohortVars = $.map($("#cohort_select option:selected"), function(elem){
			return $(elem).text();
	});

	jpsurvData.calculate.form.cohortValues = [];

	$.each(jpsurvData.calculate.form.cohortVars, function( index, value ) {
		jpsurvData.calculate.form.cohortValues.push('"'+$('#cohort_value_'+index+'_select').val()+'"');
	});
	//Sample
	$.each([ 52, 97 ], function( index, value ) {
		console.log( index + ": " + value );
	});

	// covariate
	jpsurvData.calculate.form.covariateVars = $('#covariate_select').val();
	if(jpsurvData.calculate.form.covariateVars == "None") {
		jpsurvData.calculate.form.covariateVars = '""';
	}

	// range
	jpsurvData.calculate.form.yearOfDiagnosisRange = [parseInt($('#year_of_diagnosis_start').val()), parseInt($('#year_of_diagnosis_end').val())];
	jpsurvData.calculate.form.joinPoints = parseInt($('#join_point_select').val()),

	/*
	console.log('inputAnswers');

	var obj = {
		"seerFilePrefix" : seerFilePrefix,
		"yearOfDiagnosisVarName" : yearOfDiagnosisVarName,
		"yearOfDiagnosisRange" : yearofDiagnosisRange,
		"allVars" : JSON.stringify(allVars),
		"cohortVars" : cohortVars,
		"cohortValues" : cohortValues,
		"covariateVars" : $('#covariate_select').val(),
		"numJP" : parseInt($('#join_point_select').val()),
		"outputFileName" : seerFilePrefix + ".output"
	};
	*/

	console.log("setCalculateData()");
	console.dir(jpsurvData);

	//Append the plot intervals
	append_plot_intervals(jpsurvData.calculate.form.yearOfDiagnosisRange[1] - jpsurvData.calculate.form.yearOfDiagnosisRange[0]);
	get_apc();

}

function setPlotData() {

	jpsurvData.plot.form.intervals = $('#plot-intervals').val();
	jpsurvData.plot.form.covariateVars = $('#covariate_value_select').val();
	jpsurvData.plot.static.imageId++;
	// Create a unique image id.
	get_plot();
	$('#plot-image').attr('src', '../jpsurv/tmp/plot-'+jpsurvData.tokenId+'-'+jpsurvData.plot.static.imageId+'.png')
	$('#data-set').attr('href', '../jpsurv/tmp/output-' +jpsurvData.tokenId+'.rds');
}

function file_submit() {
	//set tokenId
	jpsurvData.tokenId = parseInt(Math.random()*1000000);
	$("#upload-form").attr('action', '/jpsurvRest/stage1_upload?tokenId='+jpsurvData.tokenId);
  console.log("About to tokenId = " + jpsurvData.tokenId);

   $("#upload_file_submit").submit();

}

function get_plot() {
	$("#plot-container").hide();
	$("#spinner-plotting").show();
	console.log('get_plot');
	var params = 'jpsurvData='+JSON.stringify(jpsurvData);
	// Calling jpsurvRest
	var plot_json = JSON.parse(jpsurvRest('stage3_plot', params));

	console.log("plot_json");
	console.dir(plot_json);

	$("#spinner-plotting").hide();
	$("#plot-image").attr('src', '../jpsurv/tmp/plot-'+jpsurvData.tokenId+'.png');
	$("#plot-container").fadeIn();

}

function show_apc_table(params) {

		var apc_json = JSON.parse(jpsurvRest('stage2_calculate', params));
/*
		$('#apc_json_output').empty()
			.append($('<h2>').append('APC json').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(JSON.stringify(apc_json)));
*/
	console.log('apc_json');
	console.log(apc_json);
	$('#startYear0').empty().append(apc_json['start.year'][0]);
	$('#startYear1').empty().append(apc_json['start.year'][1]);
	$('#endYear0').empty().append(apc_json['end.year'][0]);
	$('#endYear1').empty().append(apc_json['end.year'][1]);
	$('#estimate0').empty().append(apc_json.estimate[0]);
	$('#estimate1').empty().append(apc_json.estimate[1]);

}

function get_apc() {
	/*  EXAMPLE()...

getFittedResult <- function (filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName) {

  #filePath="C:/devel/R"
  #seerFilePrefix="Breast_RelativeSurvival"
  #yearOfDiagnosisVarName="Year_of_diagnosis_1975"
  #yearofDiagnosisRange=c(1975, 2011)
  #allVar=c("Age_groups","Breast_stage","Year_of_diagnosis_1975")
  #cohortVars=c("Age_groups")
  #cohortValues=c("65+")
  #covariateVars=c("Breast_stage")
  #numJP=1
  #outputFileName="Breast_RelativeSurvival.output"

	var obj = {
		"seerFilePrefix" :"Breast_RelativeSurvival",
		"yearOfDiagnosisVarName" : "Year_of_diagnosis_1975",
		"yearofDiagnosisRange" : [1975, 2011],
		"allVar" : ["Age_groups","Breast_stage","Year_of_diagnosis_1975"],
		"cohortVars" : ["Age_groups"],
		"cohortValues" : ["00-49"],
		"covariateVars" : ["Breast_stage"],
		"numJP" : 1,
		"outputFileName" : "Breast_RelativeSurvival.output"
 };

	*/
/*
	var inputAnswers;
	// = $('#parameters').serialize();
  var yearOfDiagnosisVarName="Year_of_diagnosis_1975";  //HARD CODED...Why?


  var seerFilePrefix = "";
	var yearofDiagnosisRange = [];
	var allVar = [];
	var cohortVars = [];
	var cohortValues = [];
	var covariateVars = [];
	var numJP = 1;

  seerFilePrefix = getUrlParameter('file_control_filename');
  console.info('seerFilePrefix: '+ seerFilePrefix);

  //Find first . and trim to the left
  var seerFilePrefixIndex = seerFilePrefix.indexOf(".");
  console.log(seerFilePrefixIndex);
  if(seerFilePrefixIndex != -1) {
  		seerFilePrefix = seerFilePrefix.substring(0, seerFilePrefixIndex);
  }
  console.log(seerFilePrefix);

	cohortVars = $.map($("#cohort_select option:selected"), function(elem){
	    return $(elem).text();
	});
	$.each(cohortVars, function( index, value ) {
		cohortValues.push($('#cohort_value_'+index+'_select').val());
	});

	yearofDiagnosisRange[0] = parseInt($('#year_of_diagnosis_start').val());
	yearofDiagnosisRange[1] = parseInt($('#year_of_diagnosis_end').val());

	append_plot_intervals(yearofDiagnosisRange[1]-yearofDiagnosisRange[0]);
	console.log('inputAnswers');
	console.log(inputAnswers);

	allVars = get_cohort_covariance_variable_names();
	allVars.push(yearOfDiagnosisVarName);

	var obj = {
		"seerFilePrefix" : seerFilePrefix,
		"yearOfDiagnosisVarName" : yearOfDiagnosisVarName,
		"yearOfDiagnosisRange" : yearofDiagnosisRange,
		"allVars" : JSON.stringify(allVars),
		"cohortVars" : cohortVars,
		"cohortValues" : cohortValues,
		"covariateVars" : $('#covariate_select').val(),
		"numJP" : parseInt($('#join_point_select').val()),
		"outputFileName" : seerFilePrefix + ".output"
	};
	console.log("SERIALIZE");
	console.log($('#parameters').serialize());
	jpsurvData.calculate.form = obj;
	jpsurvData.calculate.form.serialized = $('#parameters').serializeObject();
	console.log("jpsurvData");
	console.dir(jpsurvData);
	//var calculateForm = $.text(JSON.stringify($('#parameters').serializeObject()));

	//console.log(calculateForm);
	console.log("What is obj????");
	console.dir(obj);

	var cohort_covariance_variable_names = get_cohort_covariance_variable_names();
	var params = inputAnswers +"&allVars="+ JSON.stringify(encodeURIComponent(cohort_covariance_variable_names));

	//alert('Form Answers:\n'+params.replace(/&/g, "\n"));
	//var newobj = 'obj='+JSON.stringify(encodeURIComponent(obj));
	var newobj = 'obj='+JSON.stringify(obj);
*/

	var params = 'jpsurvData='+JSON.stringify(jpsurvData);

	//jpsurvRest('calculate', newobj);
	$("#spinner").show();
	$("#apc-container").hide();
	$("#plot-container").hide();
	$("#plot-form").hide();
	//Uncomment this when data is right....
	show_apc_table(params);
	$("#spinner").hide();
	$("#plot-form").show();
	$("#apc-container").fadeIn();
}

function append_plot_intervals(max_interval) {
	$("#plot-intervals").empty();
	for(i=1;i<=max_interval;i++) {
		$("#plot-intervals").append(
			$('<option>').val(i).html(i)
			);
	}

}

function jpTrim(str, len) {
	//Trim to the right if too long...
	var newstr = str;
	if(str.length > len) {
			newstr = str.substr(0, len)+" ...";
	}

	return newstr;
}

function load_form() {
	console.log('load_form()');
	//Removing File Reader, because file is on server
	//
	//var file_control = document.getElementById('file_control').files[0];
	//var reader = new FileReader();

		//reader.onload = function(e) {
   //console.log("This may not be JSON!!!!");
	  //console.dir(text);
	  //alert(JSON.stringify(text));
	  //control_data = JSON.parse(text);
	  console.log("control data");
	  console.dir(control_data);
	  parse_diagnosis_years();
	  parse_cohort_covariance_variables();
	  build_parameter_column();
	  // The following is for demo purpose only.
	  //Temp change a title
	  $('#diagnosis_title').empty().append("Year of Diagnosis: Year of diagnosis 1975+");
	//}

	//reader.readAsText(file_control, "UTF-8");
}

function build_parameter_column() {

	set_year_of_diagnosis_select();
	set_cohort_select(Object.keys(cohort_covariance_variables));
	var covariate_options = Object.keys(cohort_covariance_variables);
	covariate_options.unshift("None");
	set_covariate_select(covariate_options);
	$("#parameters").fadeIn();
}


function parse_diagnosis_years() {
	// First we need to find the element that says "Year of Diagnosis"
	var diagnosis_row = find_year_of_diagnosis_row();
	// Then we need to read the label for the previous row, this will be the name used for the title,
	// it will ALSO be the value in the array needed to find the years

	if (diagnosis_row > 2) {
		jpsurvData.calculate.static.yearOfDiagnosisTitle = control_data.VarAllInfo.ItemValueInDic[diagnosis_row-1];
	}
	jpsurvData.calculate.static.years = control_data.VarFormatSecList[jpsurvData.calculate.static.yearOfDiagnosisTitle].ItemValueInDic;

}

function parse_cohort_covariance_variables() {
	console.log('parse_cohort_covariance_variables()');

	// First find the variables
	//  They are everything between the Page type and Year Of Diagnosis Label (noninclusive) with the VarName attribute

	var cohort_covariance_variable_names = get_cohort_covariance_variable_names();

	cohort_covariance_variables = new Object();
	for (var i=0; i< cohort_covariance_variable_names.length;i++) {
		//console.log("cohort_covariance_variable_names[i] where i ="+i+" and value is "+cohort_covariance_variable_names[i])
		var cohort_covariance_variable_values = get_cohort_covariance_variable_values(cohort_covariance_variable_names[i]);
		cohort_covariance_variables[cohort_covariance_variable_names[i]] = cohort_covariance_variable_values;
	}
}

function get_cohort_covariance_variable_names() {
	var cohort_covariance_variable_names = [];

	//var names = control_data.VarAllInfo.ItemNameInDic;
	var form_data = control_data;
	var names = control_data.VarAllInfo.ItemNameInDic;

	//Put answer in footer
	/*
	$('#footer_output')
			.append(
				$('<div>').append(JSON.stringify(form_data[0]))
			);
	*/
	var values = control_data.VarAllInfo.ItemValueInDic;
	var regex_base = /^Var\d*Base/;
	var regex_name = /^Var\d*Name/;
  //Go through Item Value and look for "Year of diagnosis"
  //Push variable names on to a list called cohort_covariance_variable_names.
	for (var i=0; i<names.length; i++) {
		//console.log('names['+i+'] = '+names[i]+', values['+i+'] = '+values[i]);
		if (regex_base.test(names[i]) && values[i] == "Year of diagnosis") break;
		if (!regex_name.test(names[i])) continue;
		if (values[i] == "Page type") continue; // Skip the Page type
		cohort_covariance_variable_names.push(values[i]);
	}
	cohort_covariance_variable_names.pop();
	//alert (JSON.stringify(cohort_covariance_variable_names));
	//console.dir(cohort_covariance_variable_names);
	return cohort_covariance_variable_names;
}

function get_cohort_covariance_variable_values(name) {
	return control_data.VarFormatSecList[name].ItemValueInDic;
}

function find_year_of_diagnosis_row() {
	var vals = control_data.VarAllInfo.ItemValueInDic;
	for (var i=0; i< vals.length; i++) {
		if (vals[i] == "Year of diagnosis") return i;
	}
	return 0;
}

function set_year_of_diagnosis_select() {

	$("#diagnosis_title").empty()
		.append(jpsurvData.calculate.static.yearOfDiagnosisTitle);

	for (i=0;i<jpsurvData.calculate.static.years.length;i++) {
		$("#year_of_diagnosis_start").append("<OPTION>"+jpsurvData.calculate.static.years[i]+"</OPTION>");
		$("#year_of_diagnosis_end").append("<OPTION>"+jpsurvData.calculate.static.years[i]+"</OPTION>");
	}
	//
	//Set last entry in year_of_diagnosis_end
	//
	//
	//Count the number of options in #year_of_diagnosis_end and select the last one.
	//
	var numberOfOptions = $('select#year_of_diagnosis_end option').length;
	$('#year_of_diagnosis_end option')[numberOfOptions-1].selected = true;

}

function set_cohort_select(cohort_options) {
	var max_size = 4;
	if (cohort_options.length < 4) max_size = cohort_options.length
	$("#cohort_select").attr("size", max_size);

	$("#cohort_select").empty();
	for (i=0;i<cohort_options.length;i++) {
		$("#cohort_select").append("<OPTION>"+cohort_options[i]+"</OPTION>");
	}
}

function set_covariate_select(covariate_options) {

	if(covariate_options.length == 0 ) {
		console.log("Covariate is length 0.");
	}
	$("#covariate_select").empty();
	$("#covariate_select_plot").empty();

	for (i=0;i<covariate_options.length;i++) {
		$("#covariate_select").append("<OPTION>"+covariate_options[i]+"</OPTION>");
		$("#covariate_select_plot").append("<OPTION>"+covariate_options[i]+"</OPTION>");
	}

}

function change_cohort_select() {
	var all_selected = $("#cohort_select").val();

	var keys =  Object.keys(cohort_covariance_variables);

	$("#cohort_sub_select").empty();
	$("#covariate_select").val('None');
	//alert('empty covariate_sub_select');
	$("#covariate_sub_select").empty();
	//alert('Is it empty?');

	if (all_selected != null) {
		for (var i=0;i<all_selected.length;i++) {
			for (var j=0;j<keys.length;j++) {
				if (all_selected[i] == keys[j])
					add_cohort_covariance_variable_select($("#cohort_sub_select"), "cohort_value_"+i, keys[j], cohort_covariance_variables[keys[j]]);
			}
		}
		var covariate_options = remove_items_from_set(keys, all_selected);
	} else {
		var covariate_options = keys;
	}
	covariate_options.unshift("None");
	set_covariate_select(covariate_options);

}

function remove_items_from_set(big_set, removed_set) {
	var new_set = [];

	for (i=0;i<big_set.length;i++) {
		if ($.inArray(big_set[i], removed_set) == -1) new_set.push(big_set[i]);
	}

//	alert ("BigSet: " + JSON.stringify(big_set)
//		 + "\nRemoved Set: " + JSON.stringify(removed_set)
//		 + "\nNew Set: " + JSON.stringify(new_set)
//		);
	return new_set;
}

function change_covariate_select() {

	var all_selected = $("#covariate_select").val();
	var keys =  Object.keys(cohort_covariance_variables);

	$("#covariate_sub_select").empty();

	console.log(all_selected);

	if (all_selected != null) {
			for (var j=0;j<keys.length;j++) {
				if (all_selected == keys[j])
					add_cohort_covariance_variable_select($("#covariate_sub_select"), "covariate_value", keys[j], cohort_covariance_variables[keys[j]]);
			}
		var covariate_options = remove_items_from_set(keys, all_selected);
	} else {
		var covariate_options = keys;
	}

/*
	if($('#covariate_select').val() == "None") {
		alert('Covariate Sub Select = none');
		$("#covariate_sub_select").empty();
	} else {
		alert('Covariate Sub Select = '+ $('#covariate_select').val());
		//add_cohort_covariance_variable_select2();
		add_cohort_covariance_variable_select($("#covariate_sub_select"), "val_"+i, keys[j], cohort_covariance_variables[keys[j]]);
/*
		for (var i=0;i<all_selected.length;i++) {
			for (var j=0;j<keys.length;j++) {
				if (all_selected[i] == keys[j])
					add_cohort_covariance_variable_select($("#covariate_sub_select"), "val_"+i, keys[j], cohort_covariance_variables[keys[j]]);
			}
		}
*/

/*
	for (var i=0;i<all_selected.length;i++) {
		for (var j=0;j<keys.length;j++) {
			if (all_selected[i] == keys[j])
				add_cohort_covariance_variable_select($("#covariate_sub_select"), "val_"+i, keys[j], cohort_covariance_variables[keys[j]]);
		}
	}
*/
	//
	//Just clear for now....
	//
	//$("#covariate_sub_select").empty();

}

function add_cohort_covariance_variable_select(field, variable_name, variable_title, values) {
	/*
	console.log("ATTEMPTING TO ADD COHORT COVARIANCE VARIABLE SELECT");
	console.log(field);
	console.log(variable_name);
	console.log(variable_title);
	console.log(values);
	*/
	//alert(field.attr('id'));

	var variable_select = $("<SELECT id='"+variable_name+"_select' name='"+variable_name+"_select'>");
	for (i=0;i<values.length;i++) {
		variable_select.append("<OPTION>"+values[i]+"</OPTION>");
	}

	var label_message = "&nbsp;&nbsp;" + variable_title + " :";

	//Label
	var label = $("<label>")
		.append(label_message)
		.attr('for',variable_name+'_select');

	field.append($("<DIV class='sub_select'>")
			.append(label)
			.append(variable_select)
			);

	if(field.attr('id') == "covariate_sub_select") {
		$("#"+variable_name+"_select").attr('multiple', '');
	}

}

function build_output_format_column() {
	$("#output_format").fadeIn();
}


function jpsurvRest(action, params) {

	var json = (function () {
    var json = null;
    //var url = '/jpsurvRest/'+action+'?'+params+'&jpsurvData='+JSON.stringify(jpsurvData);

    var url = '/jpsurvRest/'+action+'?'+encodeURI(params);
    console.warn("jpsurvRest url=");
    console.log(url);
    $.ajax({
	      'async': false,
	      'global': false,
	      'url': url,
	      'dataType': "json",
	      'success': function (data) {
	        json = data;
	      }
	    });
	    return json;
		})();

		return json;
}

function load_ajax(filename) {

	//console.log(filename);

	var json = (function () {
    var json = null;
    var url = '/jpsurv/tmp/'+filename;
    $.ajax({
	      'async': false,
	      'global': false,
	      'url': url,
	      //'dataType': "json",
	      'success': function (data) {
	        json = data;
	      }
	    });
	    return json;
	})();

	return json;

}

function getUrlParameter(sParam) {
    var sPageURL = window.location.search.substring(1);
    var sURLVariables = sPageURL.split('&');
    for (var i = 0; i < sURLVariables.length; i++)
    {
        var sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam)
        {
            return sParameterName[1];
        }
    }
}

function 	inspect(object) {
	console.log(typeof object);
	console.dir(object);

}

/**
 * objectInspector digs through a Javascript object
 * to display all its properties
 *
 * @param object - a Javascript object to inspect
 * @param result - a string of properties with datatypes
 *
 * @return result - the concatenated description of all object properties
 */
function objectInspector(object, result) {
    if (typeof object != "object")
        return "Invalid object";
    if (typeof result == "undefined")
        result = '';

    if (result.length > 50)
        return "[RECURSION TOO DEEP. ABORTING.]";

    var rows = [];
    for (var property in object) {
        var datatype = typeof object[property];

        var tempDescription = result+'"'+property+'"';
        tempDescription += ' ('+datatype+') => ';
        if (datatype == "object")
            tempDescription += 'object: '+objectInspector(object[property],result+'  ');
        else
            tempDescription += object[property];

        rows.push(tempDescription);
    }//Close for

    return rows.join(result+"\n");
}//End objectInspector

$.fn.serializeObject = function()
{
    var o = {};
    var a = this.serializeArray();
    $.each(a, function() {
        if (o[this.name] !== undefined) {
            if (!o[this.name].push) {
                o[this.name] = [o[this.name]];
            }
            o[this.name].push(this.value || '');
        } else {
            o[this.name] = this.value || '';
        }
    });
    return o;
};


