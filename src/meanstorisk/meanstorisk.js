var oTable;
var outputTable;
var giRedraw = false;
var aData;
var numberOfRows;
var uniqueKey;

var old_value;
var editing = false;
var row;
var col;
var validPrevValue = false;


var ppv_tabs = {
	"PPV": "PPV = Positive Predicted Value (PPV)",
	"cNPV": "cNPV = 1 - PPV = Compliment of the Negative Predicted Value",
	"Risk Difference": "PPV - cNPV",
	"Cases per 1000 Screened": "# of Cases Detected per 1000 Screened",
	"Cases per 1000 Positive": "# of Cases Detected per 1000 Who Screened Positive",
	"Cases per 1000 with Disease": "# of Cases Detected per 1000 with the Disease"
};


$(document).ready(function() {
	bind_calculate_button();
	bind_option_choices();
});

function bind_option_choices() {
	// I did this backwards at first.  Instead of enabling based on their option choice, have it set
	// the option if they click on one side or the other. 
	$('input[name=data_entry_option]').val(['2']);

	$(".data_entry_by_file").click(function() {
		$('input[name=data_entry_option]').val(['1']);
	});
	$(".data_entry_by_input").click(function() {
		$('input[name=data_entry_option]').val(['2']);
	});
	
/* Old Way	
	$(".data_entry_by_input").prop('disabled', true);
	$(".data_entry_by_file").prop('disabled', true);

	$('input[type="radio"]').click(function(){
		if ($(this).is(':checked')) {
			if ($(this).val() == 1) {
				$(".data_entry_by_input").prop('disabled', true);
				$(".data_entry_by_file").prop('disabled', false);
			} else if ($(this).val() == 2) {
				$(".data_entry_by_file").prop('disabled', true);
				$(".data_entry_by_input").prop('disabled', false);
			}
		}
	});	
*/
}


function bind_calculate_button() {
	$( "#calculate_button" ).click(function() {
		get_inputs_for_standard_calculation();
		get_data_stream();
	});
}

function ajax_error(jqXHR, exception)
{	
   refreshGraph(1);
   alert("ajax problem");
}


/// -----------------------------------------------
/// Computation Functions

function get_inputs_for_standard_calculation () {
	
	var mean_cases = parseFloat($("#mean_cases_input").val());
	var mean_controls = parseFloat($("#mean_controls_input").val());
	var stderr_cases = parseFloat($("#stderr_cases_input").val());
	var stderr_controls = parseFloat($("#stderr_controls_input").val());
	var N_cases = parseFloat($("#N_cases_input").val());
	var N_controls = parseFloat($("#N_controls_input").val());

	
	cases_string="" +mean_cases+","+stderr_cases+","+N_cases+""; 
	controls_string="" +mean_controls+","+stderr_controls+","+N_controls+""; 
	specificity_string="" + $("#specificity").val() + ""; 
	prevalence_string="" + $("#prevalence").val() + ""; 
		
//	alert(cases_string + "\n" + controls_string + "\n" + specificity_string + "\n" + prevalence_string);
	set_standard_inputs(mean_cases,mean_controls,stderr_cases,stderr_controls,N_cases,N_controls);
}

function set_standard_inputs(mean_cases,mean_controls,stderr_cases,stderr_controls,N_cases,N_controls) {

	// First the input values
	set_value("#mean_cases",mean_cases);	
	set_value("#mean_controls",mean_controls);	
	set_value("#stderr_cases",stderr_cases);	
	set_value("#stderr_controls",stderr_controls);	
	set_value("#N_cases",N_cases);	
	set_value("#N_controls",N_controls);	
	
	// Now the derived values
	var deviation_cases= stderr_cases * Math.sqrt(N_cases);
	set_value("#deviation_cases",deviation_cases.toPrecision(4) );
	var deviation_controls= stderr_controls * Math.sqrt(N_controls);
	set_value("#deviation_controls",deviation_controls.toPrecision(4) );
	var variance_cases= deviation_cases * deviation_cases;	
	set_value("#variance_cases",variance_cases.toPrecision(4) );
	var variance_controls= deviation_controls * deviation_controls;	
	set_value("#variance_controls",variance_controls.toPrecision(4) );
	var variance_overall = ( (N_cases * variance_cases) + (N_controls * variance_controls) )/ (N_cases + N_controls);
	set_value("#variance_overall",variance_overall.toPrecision(4) );
	
	var mean_overall = ( (N_cases * mean_cases) + (N_controls * mean_controls) )/ (N_cases + N_controls);
	set_value("#mean_overall",mean_overall.toPrecision(4) );

	var N_overall = N_cases + N_controls;
	set_value("#N_overall",N_overall.toPrecision(4) );

	var cv_cases = Math.sqrt(variance_cases) / mean_cases;
	set_value("#cv_cases",cv_cases.toPrecision(4) );
	var cv_controls = Math.sqrt(variance_controls) / mean_controls;
	set_value("#cv_controls",cv_controls.toPrecision(4) );
	var cv_overall = Math.sqrt(variance_overall) / mean_overall;
	set_value("#cv_overall",cv_overall.toPrecision(4) );
	
	var difference_in_mean = mean_cases - mean_controls;
	set_value("#mean_difference",difference_in_mean.toPrecision(4) );
	set_value("#diff_overall",difference_in_mean.toPrecision(4) );
	
	var delta = difference_in_mean / Math.sqrt(variance_overall);
	set_value("#delta_overall",delta.toPrecision(4) );

    delta_string = "c("+delta.toPrecision(4)+")";
	
}

function get_data_stream() {
	//alert(cases_string + "\n" + controls_string + "\n" + specificity_string + "\n" + prevalence_string);
    uniqueKey = (new Date()).getTime();	
    hostname = window.location.hostname;
    url = "http://" + hostname +"/meanstoriskRest/"
    $.ajax({
		type: "POST",
		url: url,
		data: {
			option:2,
			spec:specificity_string, 
			prev: prevalence_string, 
			cases: cases_string, 
			controls: controls_string, 
			unique_key: uniqueKey,
			graphkey:'input'
		},
		dataType: "json",
		success: set_data,
		error: ajax_error
	});
}

function set_data(dt) {
//	alert ("Success");
	create_tabbed_table(dt);
	draw_graph();
}

function ajax_error(dt) {
	alert("There was some problem getting the data."); 	
}

function create_tabbed_table(dt) {
//	var jsonString;
//	for (property in dt) {
//  		jsonString = dt[property];
//	}	
//	var jsonObject = $.parseJSON(jsonString);
//	alert("DT:[" + jsonObject + "]" );
	
	make_tabs();
	
	set_matrix("#tab-1", 'PPV', 'Risk of Disease after a POSITIVE Test', 'Positive Predicted Value (PPV)', 
			dt['Sensitivity Given Specificity'], dt['PPV']);	
	set_matrix("#tab-2", 'cNPV', 'Risk of Disease after a NEGATIVE Test', 'Complement of the Negative Predictive Value (cNPV)', 
			dt['Sensitivity Given Specificity'], dt['cNPV']);	
	set_matrix("#tab-3", 'PPVcNPV', 'Range of Risk after Test Results', 'PPV &minus; cNPV', 
			dt['Sensitivity Given Specificity'], dt['PPV-cNPV']);	
	set_matrix("#tab-4", 'ProgramBased', '# of Cases Detected per 1000 People Screened', 'Program &minus; Based', 
			dt['Sensitivity Given Specificity'], dt['Program-Based']);	
	set_matrix("#tab-5", 'PPVBased', '# of Cases Detected per 1000 Who are Screen Positive', 'PPV &minus; Based', 
			dt['Sensitivity Given Specificity'], dt['PPV-Based']);	
	set_matrix("#tab-6", 'SensitivityBased', '# of Cases Detected per 1000 With Disease', 'Sensitivity &minus; Based', 
			dt['Sensitivity Given Specificity'], dt['Sensitivity-Based']);	
	set_matrix("#tab-7", 'DominatedByRareDisease', '# Per 1000 Screenees Who Screen Positive', 'Dominated by Specificity for Rare Disease', 
			dt['Sensitivity Given Specificity'], dt['Dominated by Specificity for a Rare Disease']);	
	
}

function make_tabs() {
	tabs = $("<div id='tabs' style='width:1180px;margin:5px;'> </div>");
	$(".tabbed_output_panel").empty().append(tabs);
	tab_names = $("<UL> </UL>");
	tabs.append(tab_names);
	
	var index = 0;
	for (key in ppv_tabs) {
		index++;
		tab_names.append("<LI><a  style='padding:3px;' href='#tab-" + index + "' title='" + ppv_tabs[key] + "'>" + key + "</a></LI>");
		tabs.append("<DIV style='width:1180px;height:325px;' id='tab-" + index + "' > " + ppv_tabs[key] + " </div>"); 
	}
	
	tabs.tabs();
	
}

function set_matrix(tab_id, type, table_name, table_second_name, sensitivity_matrix, matrix) {
	var prevalence_values = Object.keys(matrix[0])
	var prevalence_count = prevalence_values.length;
	var specificity_count = matrix.length;
	
	
	var general_table = $("<TABLE class='table_data' style='width:94%;'></TABLE>");
	$(tab_id).empty().append(general_table);
	
	var first_header_row = $("<tr></tr>");	
	first_header_row.append("<TH class='table_data header' colspan=" +  (prevalence_count + 4)
		+ "style='background-color:#8080FF;'>" + table_name + "</TH>");
	first_header_row.appendTo(general_table);

	var second_header_row = $("<tr></tr>");	
	second_header_row.append("<TH class='table_data " + type + "_stripe' colspan='" +  (prevalence_count + 4) +"'>" 
		+ table_second_name + "</TH>");
	second_header_row.appendTo(general_table);

	var third_header_row = $("<tr></tr>");	
	third_header_row.append("<TH class='table_data header' colspan='4' style='border-right:1px solid black;'>" 
		+ "Sensitivity Given Specificity <br /> for Given Delta </TH>" );
	third_header_row.append("<TH class='table_data header' colspan='" + prevalence_count + "' >Disease Prevalence</TH>");
	third_header_row.appendTo(general_table);
	
	var header_row = $("<tr></tr>");
	header_row.attr('id', type + '_table_row_header');
	header_row.append("<TH class='table_data header'>Specificity</TD>");
	header_row.append("<TH class='table_data header'>Sensitivity</TD>");
	header_row.append("<TH class='table_data header'>LR+</TD>");
	header_row.append("<TH class='table_data header' style='border-right:1px solid black;'>LR-</TD>");
	for (var x=0;x<prevalence_count;x++) {
		header_row.append("<TH class='table_data header'>" + format_number(prevalence_values[x]) + "</TD>");
	}
	header_row.appendTo(general_table);
	
	for (var y=0;y < specificity_count;y++) {
		var row = $("<tr></tr>");
		// First do the specificity
		row.attr('id', type + '_table_row_' + x);
//		row.append("<TD class='table_data col1'>0</TD>");
//		row.append("<TD class='table_data col1'>1</TD>");
//		row.append("<TD class='table_data col1'>2</TD>");
//		row.append("<TD class='table_data col1' style='border-right:1px solid black;'>3</TD>");

		row.append("<TD class='table_data col1'>" + format_number(sensitivity_matrix[y]['Specificity']) + "</TD>");
		row.append("<TD class='table_data col1'>" + format_number(sensitivity_matrix[y]['Sensitivity']) + "</TD>");
		row.append("<TD class='table_data col1'>" + format_number(sensitivity_matrix[y]['LR+']) + "</TD>");
		row.append("<TD class='table_data col1' style='border-right:1px solid black;'>" + 
			format_number(sensitivity_matrix[y]['LR-']) + "</TD>");
		
		
		
		// Then do prevalence
		for (var x=0;x<prevalence_count;x++) {
			var prevalence_value = prevalence_values[x];
			row.append("<TD class='table_data col1'>" + format_number(matrix[y][prevalence_value]) + "</TD>");			
		}
		row.appendTo(general_table);
	}	
}


function draw_graph() {
    url = "http://" + hostname +"/meanstoriskRest/"
     graph_file = "./img/input"+uniqueKey+".png?";

     $(".graph_panel").empty().append("<IMG src='" + graph_file + "' width='390' height='390' />");
}

function set_value(field, value) {
	$(field).text("" + value);
    $(field).addClass('highlight');
    setTimeout(
        function() { $(field).removeClass('highlight'); }, 
        2000
    );		
}

function format_number(num) {
//	var intermediate = new Number(num.toPrecision(3));
//	if (num < 100 && num > 0.001) return intermediate.toString();
//	else return intermediate.toExponential();
	return num;
}

