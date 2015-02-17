var year_of_diagnosis_title = "Year fo Diagnosis 1975+";

var control_data;

var years;
//var years = [
//	                                   "1975","1976","1977","1978","1979",
//	"1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
//	"1990","1991","1982","1983","1984","1985","1986","1987","1988","1989",
//	"2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
//	"2010","2011"
//];
//Constant series of events.   On a daily events.

var cohort_covariance_variables = {
	"Age groups": ["0-49","50-65s","65+"],
	"Breast stage": ["Localized","Regional","Distant"],
	"Test group": ["val1","ValTwo","AnotherValue"]
}

$(document).ready(function() {

	var status = getUrlParameter('status');
	if(status == "OK") {
		$('#file_control_container')
				.empty()
				.append($('<div>')
					.append('<b>Data Dictionary File:</b> ' +getUrlParameter('file_control_filename'))
					);
		$('#file_data_container')
				.empty()
				.append($('<div>')
					.append('<b>Data File:</b> ' +getUrlParameter('file_data_filename'))
					);
		$('#upload_file_submit_container').remove();
		var file_control_output = load_ajax(getUrlParameter('file_control_filename'));
		//console.log(file_control_output	);

		var file_data_output = load_ajax(getUrlParameter('file_data_filename'));
		//console.log(file_data_output);
		var output_file = load_ajax(getUrlParameter('output_file'));
		//console.log(output_file);
//		alert('Updating R Output');

		$( "<a>" )
			.append('Reset')
			.attr('href', '/jpsurv')
			.insertBefore('#file_control_output');
		load_form(file_control_output);
		$( "<hr>" ).insertBefore('#file_control_output');
		$('#file_control_output').empty()
			.append($('<h2>').append('File Control Output').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(JSON.stringify(file_control_output)));

		$('#file_data_output').empty()
			.append($('<h2>').append('File Data Output').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(JSON.stringify(file_data_output)));

		$('#r_output').empty()
			.append($('<h2>').append('R Output File').addClass('pull-left'))
			.append($('<div>').css('clear', 'both'))
			.append($('<div>').append(JSON.stringify(output_file)));

	} else {
			$("#parameter_submit").on("click", build_output_format_column);
	}

	$("#cohort_select").on("change", change_cohort_select);
	$("#covariate_select").on("change", change_covariate_select);
	$("#calculate").on("click", show_graph);

});

function show_graph() {

	$("#spinner").show();
	$("#plot").hide();

	setTimeout(function(){
			$("#spinner").hide();
			$("#plot").fadeIn();
	}, 3000);
}

function load_form(file_control_output) {
	parameter_data = read_dic_file(file_control_output);
}

function read_dic_file(file_control_output) {
	//Removing File Reader, because file is on server
	//
	//var file_control = document.getElementById('file_control').files[0];
	//var reader = new FileReader();

	//reader.onload = function(e) {
	  var text = file_control_output;
	  console.log("This may not be JSON!!!!");
	  console.log(text);
	  //alert(JSON.stringify(text));
	  control_data = JSON.parse(text);
	  console.dir(control_data);
	  parse_diagnosis_years();
	  parse_cohort_covariance_variables();
	  build_parameter_column();
	  // The following is for demo purpose only.
	  //Temp change a title
	  $('#diagnosis_title').empty().append("Year of Diagnosis: Year of diagnosis 1975+");
	  $('#year_of_diagnosis_end option:contains("2011")').attr('selected', 'selected');
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

	var diagnosis_label = 'unknown';
	if (diagnosis_row > 2) {
		diagnosis_label = control_data.VarAllInfo.ItemValueInDic[diagnosis_row-1];
	}

	year_of_diagnosis_title = diagnosis_label;
	years = control_data.VarFormatSecList[diagnosis_label].ItemValueInDic;

//	alert(JSON.stringify(years));
//	alert (control_data.VarAllInfo.ItemNameInDic[0]+"->"+control_data.VarAllInfo.ItemValueInDic[0]);
}

function parse_cohort_covariance_variables() {

	// First find the variables
	//  They are everything between the Page type and Year Of Diagnosis Label (noninclusive) with the VarName attribute

	var cohort_covariance_variable_names = get_cohort_covariance_variable_names();

	cohort_covariance_variables = new Object();
	for (var i=0; i< cohort_covariance_variable_names.length;i++) {
		var cohort_covariance_variable_values = get_cohort_covariance_variable_values(cohort_covariance_variable_names[i]);
		cohort_covariance_variables[cohort_covariance_variable_names[i]] = cohort_covariance_variable_values;
	}
}

function get_cohort_covariance_variable_names() {
	var cohort_covariance_variable_names = [];

	//var names = control_data.VarAllInfo.ItemNameInDic;
	var form_data = get_form_data();
	var names = form_data[0].VarAllInfo.ItemNameInDic;

	//Put answer in footer
	/*
	$('#footer_output')
			.append(
				$('<div>').append(JSON.stringify(form_data[0]))
			);
	*/
	var values = form_data[0].VarAllInfo.ItemValueInDic;
	var regex_base = /^Var\d*Base/;
	var regex_name = /^Var\d*Name/;

	for (var i=0; i<names.length; i++) {
		if (regex_base.test(names[i]) && values[i] == "Year of diagnosis") break;
		if (!regex_name.test(names[i])) continue;
		if (values[i] == "Page type") continue; // Skip the Page type
		cohort_covariance_variable_names.push(values[i]);
	}
	cohort_covariance_variable_names.pop();
//	alert (JSON.stringify(cohort_covariance_variable_names));
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
	$("#diagnosis_title").empty().append(year_of_diagnosis_title);

	for (i=0;i<years.length;i++) {
		$("#year_of_diagnosis_start").append("<OPTION>"+years[i]+"</OPTION>");
		$("#year_of_diagnosis_end").append("<OPTION>"+years[i]+"</OPTION>");
	}
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
	var max_size = 4;
	if (covariate_options.length < 4) max_size = covariate_options.length
	$("#covariate_select").attr("size", max_size);

	$("#covariate_select").empty();
	for (i=0;i<covariate_options.length;i++) {
		$("#covariate_select").append("<OPTION>"+covariate_options[i]+"</OPTION>");
	}
	$("#covariate_sub_select").empty();
}

function change_cohort_select() {
	var all_selected = $("#cohort_select").val();

	var keys =  Object.keys(cohort_covariance_variables);

	$("#cohort_sub_select").empty();
	if (all_selected != null) {
		for (var i=0;i<all_selected.length;i++) {
			for (var j=0;j<keys.length;j++) {
				if (all_selected[i] == keys[j])
					add_cohort_covariance_variable_select($("#cohort_sub_select"), "val_"+i, keys[j], cohort_covariance_variables[keys[j]]);
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

	for (var i=0;i<all_selected.length;i++) {
		for (var j=0;j<keys.length;j++) {
			if (all_selected[i] == keys[j])
				add_cohort_covariance_variable_select($("#covariate_sub_select"), "val_"+i, keys[j], cohort_covariance_variables[keys[j]]);
		}
	}
}

function add_cohort_covariance_variable_select(field, variable_name, variable_title, values) {
	var variable_select = $("<SELECT id='"+variable_name+"_select'>");
	for (i=0;i<values.length;i++) {
		variable_select.append("<OPTION>"+values[i]+"</OPTION>");
	}
	field.append($("<DIV class='sub_select'>")
		.append("&nbsp;&nbsp;" + variable_title + " : ")
		.append(variable_select));

}

function build_output_format_column() {
	$("#output_format").fadeIn();
}

function get_form_data() {

	var json = (function () {
    var json = null;
    var url = '/jpsurvRest/get_form';
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

	console.log(filename);

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
