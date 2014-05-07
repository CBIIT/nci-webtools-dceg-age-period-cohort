var mean_cases, mean_controls;
var stderr_cases, stderr_controls;
var N_cases, N_controls;

var specificity_1 = 0.8;
var specificity_2 = 0.9;
var specificity_3 = 0.95;
var specificity_4 = 0.99;
var specificity_5 = 0.995;

var delta = 2.32; // Initial Value

$(document).ready(function() { 

	$( "#compute-delta-button" ).button().click(function() {
		get_inputs_for_standard_calculation ()
	});

	$( "#compute-graph-button" ).button().click(function() {
		get_data_stream(1);
	});
	
});


function get_inputs_for_standard_calculation () {
	
	mean_cases = parseFloat($("#mean-cases-input").val());
	mean_controls = parseFloat($("#mean-controls-input").val());
	stderr_cases = parseFloat($("#stderr-cases-input").val());
	stderr_controls = parseFloat($("#stderr-controls-input").val());
	N_cases = parseFloat($("#N-cases-input").val());
	N_controls = parseFloat($("#N-controls-input").val());

	
	set_standard_inputs(mean_cases,mean_controls,stderr_cases,stderr_controls,N_cases,N_controls);
}

function set_standard_inputs(mean_cases,mean_controls,stderr_cases,stderr_controls,N_cases,N_controls) {

	// First the input values
	set_value("#mean-cases",mean_cases);	
	set_value("#mean-controls",mean_controls);	
	set_value("#stderr-cases",stderr_cases);	
	set_value("#stderr-controls",stderr_controls);	
	set_value("#N-cases",N_cases);	
	set_value("#N-controls",N_controls);	
	
	// Now the derived values
	var deviation_cases= stderr_cases * Math.sqrt(N_cases);
	set_value("#stddev-cases",deviation_cases.toPrecision(4) );
	var deviation_controls= stderr_controls * Math.sqrt(N_controls);
	set_value("#stddev-controls",deviation_controls.toPrecision(4) );
	var variance_cases= deviation_cases * deviation_cases;	
	set_value("#var-cases",variance_cases.toPrecision(4) );
	var variance_controls= deviation_controls * deviation_controls;	
	set_value("#var-controls",variance_controls.toPrecision(4) );
	var variance_overall = ( (N_cases * variance_cases) + (N_controls * variance_controls) )/ (N_cases + N_controls);
	set_value("#var-overall",variance_overall.toPrecision(4) );
	
	var mean_overall = ( (N_cases * mean_cases) + (N_controls * mean_controls) )/ (N_cases + N_controls);
	set_value("#mean-overall",mean_overall.toPrecision(4) );

	var N_overall = N_cases + N_controls;
	set_value("#N-overall",N_overall.toPrecision(4) );

	var cv_cases = Math.sqrt(variance_cases) / mean_cases;
	set_value("#cv-cases",cv_cases.toPrecision(4) );
	var cv_controls = Math.sqrt(variance_controls) / mean_controls;
	set_value("#cv-controls",cv_controls.toPrecision(4) );
	var cv_overall = Math.sqrt(variance_overall) / mean_overall;
	set_value("#cv-overall",cv_overall.toPrecision(4) );
	
	var difference_in_mean = mean_cases - mean_controls;
	set_value("#mean-diff",difference_in_mean.toPrecision(4) );
	set_value("#diff-overall",difference_in_mean.toPrecision(4) );
	
	delta = difference_in_mean / Math.sqrt(variance_overall);
	set_value("#delta",delta.toPrecision(4) );
	set_value("#delta-title",delta.toPrecision(4) );

//	delta_string = "c("+delta.toPrecision(4)+")";
}

function set_value(field, value) {
	$(field).text("" + value);
    $(field).addClass('highlight');
    setTimeout(
        function() { $(field).removeClass('highlight'); }, 
        2000
    );		
}

function get_data_stream() {
	var cases_string="c(" +mean_cases+","+stderr_cases+","+N_cases+")"; 
	var controls_string="c(" +mean_controls+","+stderr_controls+","+N_controls+")"; 
	var specificity_string="c(" +specificity_1+","+specificity_2+","+specificity_3+","+specificity_4+","+specificity_5+")"; 
	var prevalence_string="c(0.1,0.01,0.001,0.0001,0.00001,0.000001)"; 
    var delta_string = "c("+delta.toPrecision(4)+")";

	//alert(cases_string + "\n" + controls_string + "\n" + specificity_string + "\n" + prevalence_string);
	uniqueKey = (new Date()).getTime();	
	hostname = window.location.hostname;
	$.ajax({
		type: "GET",
		url: "http://"+hostname+":8780/jsonp/DeltaSpPpv",
		data: {cases: cases_string, controls: controls_string, specificity:specificity_string, prevalence: prevalence_string, delta: delta_string, spec_min: 0, spec_max: 1, unique_key: uniqueKey},
		dataType: "jsonp",
		success: set_data,
		error: ajax_error
	});
}

function set_data(dt) {
	var jsonString;
	for (property in dt) {
  		jsonString = dt[property];
	}	
//	alert (jsonString);
	var jsonObject = $.parseJSON(jsonString);
	
	//alert(jsonObject.DrawGraph[0][0])
	if (jsonObject.DrawGraph[0][0]==0)
	{
		refreshGraph(jsonObject.DrawGraph[0][0])
	}
	else
	{
       	newDelta = (jsonObject.calculatedDelta[0][0]);
//		set_value("#delta_overall",newDelta.toPrecision(4) );
//		set_specificity_on_all_matrices(jsonObject.SensitivityGivenSpecificity[0]);
//		set_PPV_matrix(jsonObject.PPV[0]);	
//		set_cNPV_matrix(jsonObject.cNPV[0]);	
//		set_PPVcNPV_matrix(jsonObject.PPVcNPV[0]);	
//		set_ProgramBased_matrix(jsonObject.ProgramBased[0]);	
//		set_PPVBased_matrix(jsonObject.PPVBased[0]);	
//		set_SensitivityBased_matrix(jsonObject.SensitivityBased[0]);	
    	refreshGraph(jsonObject.DrawGraph[0][0])
	}
	
}

function ajax_error() {
	alert("There was some problem getting the data."); 	
}

function refreshGraph(drawgraph) {
   if (drawgraph == 1) {
         graph_file = "../"+uniqueKey+"rplot.png?";
   } else {
         graph_file = "../images/fail-message.jpg?";
   }

   d = new Date();
   $("#graph_image").attr("src", graph_file+d.getTime());
}
