$(document).ready(function() { 

//	$( "#tabs" ).tabs().addClass( "ui-tabs-vertical ui-helper-clearfix" );
//	$( "#tabs li" ).removeClass( "ui-corner-top" ).addClass( "ui-corner-left" );
	$( "#tabs" ).tabs();

/* 
	$("#update_inputs").click(function(){
		$("#update_inputs_dialog").dialog({					
			width: 600,
			modal: true,
			close: function(event, ui) {
				$("#dialog").remove();
			}
		});
	});
*/

	$( "#update_inputs_dialog" ).dialog({
		autoOpen: false,
		height: 360,
		width: 350,
		modal: true,
		buttons: {
			"Calculate": function() {
				get_inputs_for_standard_calculation();
				get_data_stream();
				$( this ).dialog( "close" );
			},
			Cancel: function() {
				$( this ).dialog( "close" );
			}
		},
		close: function() {
		}
	});

	$( "#update_inputs_button" ).button().click(function() {
		$( "#update_inputs_dialog" ).dialog( "open" );
	});
});

function get_inputs_for_standard_calculation () {
	
	var mean_cases = parseFloat($("#mean_cases_input").val());
	var mean_controls = parseFloat($("#mean_controls_input").val());
	var stderr_cases = parseFloat($("#stderr_cases_input").val());
	var stderr_controls = parseFloat($("#stderr_controls_input").val());
	var N_cases = parseFloat($("#N_cases_input").val());
	var N_controls = parseFloat($("#N_controls_input").val());

	var specificity_1 = parseFloat($("#specificity_input_1").val());
	var specificity_2 = parseFloat($("#specificity_input_2").val());
	var specificity_3 = parseFloat($("#specificity_input_3").val());
	var specificity_4 = parseFloat($("#specificity_input_4").val());
	var specificity_5 = parseFloat($("#specificity_input_5").val());
	
	cases_string="c(" +mean_cases+","+stderr_cases+","+N_cases+")"; 
	controls_string="c(" +mean_controls+","+stderr_controls+","+N_controls+")"; 
	specificity_string="c(" +specificity_1+","+specificity_2+","+specificity_3+","+specificity_4+","+specificity_5+")"; 
	prevalence_string="c(0.1,0.01,0.001,0.0001,0.00001,0.000001)"; 
		
//	alert(cases_string + "\n" + controls_string + "\n" + specificity_string + "\n" + prevalence_string);
	set_standard_inputs(mean_cases,mean_controls,stderr_cases,stderr_controls,N_cases,N_controls,
		specificity_1, specificity_2, specificity_3, specificity_4, specificity_5);
}

function set_standard_inputs(mean_cases,mean_controls,stderr_cases,stderr_controls,N_cases,N_controls,
		specificity_1, specificity_2, specificity_3, specificity_4, specificity_5) {

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
	
	set_value("#specificity_1",specificity_1);
	set_value("#specificity_2",specificity_2);
	set_value("#specificity_3",specificity_3);
	set_value("#specificity_4",specificity_4);
	set_value("#specificity_5",specificity_5);
	

}

function set_value(field, value) {
	$(field).text("" + value);
    $(field).addClass('highlight');
    setTimeout(
        function() { $(field).removeClass('highlight'); }, 
        2000
    );		
}


function refreshGraph() {
   alert("refresh Graph ")
   d = new Date();
   $("#SpecGraph").attr("src", "../rplot.jpg?"+d.getTime());
}


function get_data_stream() {
	alert(cases_string + "\n" + controls_string + "\n" + specificity_string + "\n" + prevalence_string);
	
	$.ajax({
		type: "GET",
		url: "http://127.0.0.1:5000/todo/api/v1.0/DeltaSpecPpv",
		data: {cases: cases_string, controls: controls_string, specificity:specificity_string, prevalence: prevalence_string, delta: delta_string },
		dataType: "json",
		success: set_data,
		error: ajax_error
	});
}

function set_data(dt) {
        var jsonObject = JSON.parse(dt.returnstring);
	set_specificity_on_all_matrices(jsonObject.SensitivityGivenSpecificity[0]);
	set_PPV_matrix(jsonObject.PPV[0]);	
	set_cNPV_matrix(jsonObject.cNPV[0]);	
	set_PPVcNPV_matrix(jsonObject.PPVcNPV[0]);	
	set_ProgramBased_matrix(jsonObject.ProgramBased[0]);	
	set_PPVBased_matrix(jsonObject.PPVBased[0]);	
	set_SensitivityBased_matrix(jsonObject.SensitivityBased[0]);	
	set_DominatedByRareDisease_matrix(jsonObject.DominatedByRareDisease[0]);
        refreshGraph()
	
	set_specificity_on_all_matrices(); // Hard coded for now
}

function ajax_error() {
	alert("There was some problem getting the data."); 	
}

function format_number(num) {
	var intermediate = new Number(num.toPrecision(3));
        return intermediate.toString();
	//if (num < 100 && num > 0.001) return intermediate.toString();
	//else return intermediate.toExponential();
//	if (num<0.01) return (num.toPrecision(3)).toExponential();
//	else return num.toPrecision(3);	
}

function set_PPV_matrix(matrix) {
	$("#PPV_0_0").html(format_number(matrix[0][0]));
	$("#PPV_0_1").html(format_number(matrix[0][1]));
	$("#PPV_0_2").html(format_number(matrix[0][2]));
	$("#PPV_0_3").html(format_number(matrix[0][3]));
	$("#PPV_0_4").html(format_number(matrix[0][4]));
	$("#PPV_0_5").html(format_number(matrix[0][5]));	

	$("#PPV_1_0").html(format_number(matrix[1][0]));
	$("#PPV_1_1").html(format_number(matrix[1][1]));
	$("#PPV_1_2").html(format_number(matrix[1][2]));
	$("#PPV_1_3").html(format_number(matrix[1][3]));
	$("#PPV_1_4").html(format_number(matrix[1][4]));
	$("#PPV_1_5").html(format_number(matrix[1][5]));	

	$("#PPV_2_0").html(format_number(matrix[2][0]));
	$("#PPV_2_1").html(format_number(matrix[2][1]));
	$("#PPV_2_2").html(format_number(matrix[2][2]));
	$("#PPV_2_3").html(format_number(matrix[2][3]));
	$("#PPV_2_4").html(format_number(matrix[2][4]));
	$("#PPV_2_5").html(format_number(matrix[2][5]));	

	$("#PPV_3_0").html(format_number(matrix[3][0]));
	$("#PPV_3_1").html(format_number(matrix[3][1]));
	$("#PPV_3_2").html(format_number(matrix[3][2]));
	$("#PPV_3_3").html(format_number(matrix[3][3]));
	$("#PPV_3_4").html(format_number(matrix[3][4]));
	$("#PPV_3_5").html(format_number(matrix[3][5]));	

	$("#PPV_4_0").html(format_number(matrix[4][0]));
	$("#PPV_4_1").html(format_number(matrix[4][1]));
	$("#PPV_4_2").html(format_number(matrix[4][2]));
	$("#PPV_4_3").html(format_number(matrix[4][3]));
	$("#PPV_4_4").html(format_number(matrix[4][4]));
	$("#PPV_4_5").html(format_number(matrix[4][5]));	
}

function set_cNPV_matrix(matrix) {
	$("#cNPV_0_0").html(format_number(matrix[0][0]));
	$("#cNPV_0_1").html(format_number(matrix[0][1]));
	$("#cNPV_0_2").html(format_number(matrix[0][2]));
	$("#cNPV_0_3").html(format_number(matrix[0][3]));
	$("#cNPV_0_4").html(format_number(matrix[0][4]));
	$("#cNPV_0_5").html(format_number(matrix[0][5]));	

	$("#cNPV_1_0").html(format_number(matrix[1][0]));
	$("#cNPV_1_1").html(format_number(matrix[1][1]));
	$("#cNPV_1_2").html(format_number(matrix[1][2]));
	$("#cNPV_1_3").html(format_number(matrix[1][3]));
	$("#cNPV_1_4").html(format_number(matrix[1][4]));
	$("#cNPV_1_5").html(format_number(matrix[1][5]));	

	$("#cNPV_2_0").html(format_number(matrix[2][0]));
	$("#cNPV_2_1").html(format_number(matrix[2][1]));
	$("#cNPV_2_2").html(format_number(matrix[2][2]));
	$("#cNPV_2_3").html(format_number(matrix[2][3]));
	$("#cNPV_2_4").html(format_number(matrix[2][4]));
	$("#cNPV_2_5").html(format_number(matrix[2][5]));	

	$("#cNPV_3_0").html(format_number(matrix[3][0]));
	$("#cNPV_3_1").html(format_number(matrix[3][1]));
	$("#cNPV_3_2").html(format_number(matrix[3][2]));
	$("#cNPV_3_3").html(format_number(matrix[3][3]));
	$("#cNPV_3_4").html(format_number(matrix[3][4]));
	$("#cNPV_3_5").html(format_number(matrix[3][5]));	

	$("#cNPV_4_0").html(format_number(matrix[4][0]));
	$("#cNPV_4_1").html(format_number(matrix[4][1]));
	$("#cNPV_4_2").html(format_number(matrix[4][2]));
	$("#cNPV_4_3").html(format_number(matrix[4][3]));
	$("#cNPV_4_4").html(format_number(matrix[4][4]));
	$("#cNPV_4_5").html(format_number(matrix[4][5]));	
}

function set_PPVcNPV_matrix(matrix) {
	$("#PPVcNPV_0_0").html(format_number(matrix[0][0]));
	$("#PPVcNPV_0_1").html(format_number(matrix[0][1]));
	$("#PPVcNPV_0_2").html(format_number(matrix[0][2]));
	$("#PPVcNPV_0_3").html(format_number(matrix[0][3]));
	$("#PPVcNPV_0_4").html(format_number(matrix[0][4]));
	$("#PPVcNPV_0_5").html(format_number(matrix[0][5]));	

	$("#PPVcNPV_1_0").html(format_number(matrix[1][0]));
	$("#PPVcNPV_1_1").html(format_number(matrix[1][1]));
	$("#PPVcNPV_1_2").html(format_number(matrix[1][2]));
	$("#PPVcNPV_1_3").html(format_number(matrix[1][3]));
	$("#PPVcNPV_1_4").html(format_number(matrix[1][4]));
	$("#PPVcNPV_1_5").html(format_number(matrix[1][5]));	

	$("#PPVcNPV_2_0").html(format_number(matrix[2][0]));
	$("#PPVcNPV_2_1").html(format_number(matrix[2][1]));
	$("#PPVcNPV_2_2").html(format_number(matrix[2][2]));
	$("#PPVcNPV_2_3").html(format_number(matrix[2][3]));
	$("#PPVcNPV_2_4").html(format_number(matrix[2][4]));
	$("#PPVcNPV_2_5").html(format_number(matrix[2][5]));	

	$("#PPVcNPV_3_0").html(format_number(matrix[3][0]));
	$("#PPVcNPV_3_1").html(format_number(matrix[3][1]));
	$("#PPVcNPV_3_2").html(format_number(matrix[3][2]));
	$("#PPVcNPV_3_3").html(format_number(matrix[3][3]));
	$("#PPVcNPV_3_4").html(format_number(matrix[3][4]));
	$("#PPVcNPV_3_5").html(format_number(matrix[3][5]));	

	$("#PPVcNPV_4_0").html(format_number(matrix[4][0]));
	$("#PPVcNPV_4_1").html(format_number(matrix[4][1]));
	$("#PPVcNPV_4_2").html(format_number(matrix[4][2]));
	$("#PPVcNPV_4_3").html(format_number(matrix[4][3]));
	$("#PPVcNPV_4_4").html(format_number(matrix[4][4]));
	$("#PPVcNPV_4_5").html(format_number(matrix[4][5]));	
}

function set_ProgramBased_matrix(matrix) {
	$("#ProgramBased_0_0").html(format_number(matrix[0][0]));
	$("#ProgramBased_0_1").html(format_number(matrix[0][1]));
	$("#ProgramBased_0_2").html(format_number(matrix[0][2]));
	$("#ProgramBased_0_3").html(format_number(matrix[0][3]));
	$("#ProgramBased_0_4").html(format_number(matrix[0][4]));
	$("#ProgramBased_0_5").html(format_number(matrix[0][5]));	

	$("#ProgramBased_1_0").html(format_number(matrix[1][0]));
	$("#ProgramBased_1_1").html(format_number(matrix[1][1]));
	$("#ProgramBased_1_2").html(format_number(matrix[1][2]));
	$("#ProgramBased_1_3").html(format_number(matrix[1][3]));
	$("#ProgramBased_1_4").html(format_number(matrix[1][4]));
	$("#ProgramBased_1_5").html(format_number(matrix[1][5]));	

	$("#ProgramBased_2_0").html(format_number(matrix[2][0]));
	$("#ProgramBased_2_1").html(format_number(matrix[2][1]));
	$("#ProgramBased_2_2").html(format_number(matrix[2][2]));
	$("#ProgramBased_2_3").html(format_number(matrix[2][3]));
	$("#ProgramBased_2_4").html(format_number(matrix[2][4]));
	$("#ProgramBased_2_5").html(format_number(matrix[2][5]));	

	$("#ProgramBased_3_0").html(format_number(matrix[3][0]));
	$("#ProgramBased_3_1").html(format_number(matrix[3][1]));
	$("#ProgramBased_3_2").html(format_number(matrix[3][2]));
	$("#ProgramBased_3_3").html(format_number(matrix[3][3]));
	$("#ProgramBased_3_4").html(format_number(matrix[3][4]));
	$("#ProgramBased_3_5").html(format_number(matrix[3][5]));	

	$("#ProgramBased_4_0").html(format_number(matrix[4][0]));
	$("#ProgramBased_4_1").html(format_number(matrix[4][1]));
	$("#ProgramBased_4_2").html(format_number(matrix[4][2]));
	$("#ProgramBased_4_3").html(format_number(matrix[4][3]));
	$("#ProgramBased_4_4").html(format_number(matrix[4][4]));
	$("#ProgramBased_4_5").html(format_number(matrix[4][5]));	
}

function set_PPVBased_matrix(matrix) {
	$("#PPVBased_0_0").html(format_number(matrix[0][0]));
	$("#PPVBased_0_1").html(format_number(matrix[0][1]));
	$("#PPVBased_0_2").html(format_number(matrix[0][2]));
	$("#PPVBased_0_3").html(format_number(matrix[0][3]));
	$("#PPVBased_0_4").html(format_number(matrix[0][4]));
	$("#PPVBased_0_5").html(format_number(matrix[0][5]));	

	$("#PPVBased_1_0").html(format_number(matrix[1][0]));
	$("#PPVBased_1_1").html(format_number(matrix[1][1]));
	$("#PPVBased_1_2").html(format_number(matrix[1][2]));
	$("#PPVBased_1_3").html(format_number(matrix[1][3]));
	$("#PPVBased_1_4").html(format_number(matrix[1][4]));
	$("#PPVBased_1_5").html(format_number(matrix[1][5]));	

	$("#PPVBased_2_0").html(format_number(matrix[2][0]));
	$("#PPVBased_2_1").html(format_number(matrix[2][1]));
	$("#PPVBased_2_2").html(format_number(matrix[2][2]));
	$("#PPVBased_2_3").html(format_number(matrix[2][3]));
	$("#PPVBased_2_4").html(format_number(matrix[2][4]));
	$("#PPVBased_2_5").html(format_number(matrix[2][5]));	

	$("#PPVBased_3_0").html(format_number(matrix[3][0]));
	$("#PPVBased_3_1").html(format_number(matrix[3][1]));
	$("#PPVBased_3_2").html(format_number(matrix[3][2]));
	$("#PPVBased_3_3").html(format_number(matrix[3][3]));
	$("#PPVBased_3_4").html(format_number(matrix[3][4]));
	$("#PPVBased_3_5").html(format_number(matrix[3][5]));	

	$("#PPVBased_4_0").html(format_number(matrix[4][0]));
	$("#PPVBased_4_1").html(format_number(matrix[4][1]));
	$("#PPVBased_4_2").html(format_number(matrix[4][2]));
	$("#PPVBased_4_3").html(format_number(matrix[4][3]));
	$("#PPVBased_4_4").html(format_number(matrix[4][4]));
	$("#PPVBased_4_5").html(format_number(matrix[4][5]));	
}


function set_SensitivityBased_matrix(matrix) {
	$("#SensitivityBased_0_0").html(format_number(matrix[0][0]));
	$("#SensitivityBased_0_1").html(format_number(matrix[0][1]));
	$("#SensitivityBased_0_2").html(format_number(matrix[0][2]));
	$("#SensitivityBased_0_3").html(format_number(matrix[0][3]));
	$("#SensitivityBased_0_4").html(format_number(matrix[0][4]));
	$("#SensitivityBased_0_5").html(format_number(matrix[0][5]));	

	$("#SensitivityBased_1_0").html(format_number(matrix[1][0]));
	$("#SensitivityBased_1_1").html(format_number(matrix[1][1]));
	$("#SensitivityBased_1_2").html(format_number(matrix[1][2]));
	$("#SensitivityBased_1_3").html(format_number(matrix[1][3]));
	$("#SensitivityBased_1_4").html(format_number(matrix[1][4]));
	$("#SensitivityBased_1_5").html(format_number(matrix[1][5]));	

	$("#SensitivityBased_2_0").html(format_number(matrix[2][0]));
	$("#SensitivityBased_2_1").html(format_number(matrix[2][1]));
	$("#SensitivityBased_2_2").html(format_number(matrix[2][2]));
	$("#SensitivityBased_2_3").html(format_number(matrix[2][3]));
	$("#SensitivityBased_2_4").html(format_number(matrix[2][4]));
	$("#SensitivityBased_2_5").html(format_number(matrix[2][5]));	

	$("#SensitivityBased_3_0").html(format_number(matrix[3][0]));
	$("#SensitivityBased_3_1").html(format_number(matrix[3][1]));
	$("#SensitivityBased_3_2").html(format_number(matrix[3][2]));
	$("#SensitivityBased_3_3").html(format_number(matrix[3][3]));
	$("#SensitivityBased_3_4").html(format_number(matrix[3][4]));
	$("#SensitivityBased_3_5").html(format_number(matrix[3][5]));	

	$("#SensitivityBased_4_0").html(format_number(matrix[4][0]));
	$("#SensitivityBased_4_1").html(format_number(matrix[4][1]));
	$("#SensitivityBased_4_2").html(format_number(matrix[4][2]));
	$("#SensitivityBased_4_3").html(format_number(matrix[4][3]));
	$("#SensitivityBased_4_4").html(format_number(matrix[4][4]));
	$("#SensitivityBased_4_5").html(format_number(matrix[4][5]));	
}

function set_DominatedByRareDisease_matrix(matrix) {
	$("#DominatedByRareDisease_0_0").html(format_number(matrix[0][0]));
	$("#DominatedByRareDisease_0_1").html(format_number(matrix[0][1]));
	$("#DominatedByRareDisease_0_2").html(format_number(matrix[0][2]));
	$("#DominatedByRareDisease_0_3").html(format_number(matrix[0][3]));
	$("#DominatedByRareDisease_0_4").html(format_number(matrix[0][4]));
	$("#DominatedByRareDisease_0_5").html(format_number(matrix[0][5]));	

	$("#DominatedByRareDisease_1_0").html(format_number(matrix[1][0]));
	$("#DominatedByRareDisease_1_1").html(format_number(matrix[1][1]));
	$("#DominatedByRareDisease_1_2").html(format_number(matrix[1][2]));
	$("#DominatedByRareDisease_1_3").html(format_number(matrix[1][3]));
	$("#DominatedByRareDisease_1_4").html(format_number(matrix[1][4]));
	$("#DominatedByRareDisease_1_5").html(format_number(matrix[1][5]));	

	$("#DominatedByRareDisease_2_0").html(format_number(matrix[2][0]));
	$("#DominatedByRareDisease_2_1").html(format_number(matrix[2][1]));
	$("#DominatedByRareDisease_2_2").html(format_number(matrix[2][2]));
	$("#DominatedByRareDisease_2_3").html(format_number(matrix[2][3]));
	$("#DominatedByRareDisease_2_4").html(format_number(matrix[2][4]));
	$("#DominatedByRareDisease_2_5").html(format_number(matrix[2][5]));	

	$("#DominatedByRareDisease_3_0").html(format_number(matrix[3][0]));
	$("#DominatedByRareDisease_3_1").html(format_number(matrix[3][1]));
	$("#DominatedByRareDisease_3_2").html(format_number(matrix[3][2]));
	$("#DominatedByRareDisease_3_3").html(format_number(matrix[3][3]));
	$("#DominatedByRareDisease_3_4").html(format_number(matrix[3][4]));
	$("#DominatedByRareDisease_3_5").html(format_number(matrix[3][5]));	

	$("#DominatedByRareDisease_4_0").html(format_number(matrix[4][0]));
	$("#DominatedByRareDisease_4_1").html(format_number(matrix[4][1]));
	$("#DominatedByRareDisease_4_2").html(format_number(matrix[4][2]));
	$("#DominatedByRareDisease_4_3").html(format_number(matrix[4][3]));
	$("#DominatedByRareDisease_4_4").html(format_number(matrix[4][4]));
	$("#DominatedByRareDisease_4_5").html(format_number(matrix[4][5]));	
}

function set_specificity_on_all_matrices (matrix) {
	specificity_1_String = parseFloat($("#specificity_input_1").val())*100+"%";
	specificity_2_String = parseFloat($("#specificity_input_2").val())*100+"%";
	specificity_3_String = parseFloat($("#specificity_input_3").val())*100+"%";
	specificity_4_String = parseFloat($("#specificity_input_4").val())*100+"%";
	specificity_5_String = parseFloat($("#specificity_input_5").val())*100+"%";

	//$("#PPV_0_0").html(format_number(matrix[0][0]));
	//$("#PPV_0_1").html(format_number(matrix[0][1]));
	//$("#PPV_0_2").html(format_number(matrix[0][2]));


	//$("#PPV_1_0").html(format_number(matrix[1][0]));
	//$("#PPV_1_1").html(format_number(matrix[1][1]));
	//$("#PPV_1_2").html(format_number(matrix[1][2]));
	
	$("#PPV_Specificity_0").html(format_number(matrix[0][0]));
	$("#PPV_Specificity_1").html(format_number(matrix[1][0]));
	$("#PPV_Specificity_2").html(format_number(matrix[2][0]));
	$("#PPV_Specificity_3").html(format_number(matrix[3][0]));
	$("#PPV_Specificity_4").html(format_number(matrix[4][0]));

	$("#PPV_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#PPV_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#PPV_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#PPV_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#PPV_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#PPV_LRplus_0").html(format_number(matrix[0][2]));
	$("#PPV_LRplus_1").html(format_number(matrix[1][2]));
	$("#PPV_LRplus_2").html(format_number(matrix[2][2]));
	$("#PPV_LRplus_3").html(format_number(matrix[3][2]));
	$("#PPV_LRplus_4").html(format_number(matrix[4][2]));

	$("#PPV_LRminus_0").html(format_number(matrix[0][3]));
	$("#PPV_LRminus_1").html(format_number(matrix[1][3]));
	$("#PPV_LRminus_2").html(format_number(matrix[2][3]));
	$("#PPV_LRminus_3").html(format_number(matrix[3][3]));
	$("#PPV_LRminus_4").html(format_number(matrix[4][3]));



	$("#cNPV_Specificity_0").html(format_number(matrix[0][0]));
	$("#cNPV_Specificity_1").html(format_number(matrix[1][0]));
	$("#cNPV_Specificity_2").html(format_number(matrix[2][0]));
	$("#cNPV_Specificity_3").html(format_number(matrix[3][0]));
	$("#cNPV_Specificity_4").html(format_number(matrix[4][0]));

	$("#cNPV_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#cNPV_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#cNPV_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#cNPV_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#cNPV_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#cNPV_LRplus_0").html(format_number(matrix[0][2]));
	$("#cNPV_LRplus_1").html(format_number(matrix[1][2]));
	$("#cNPV_LRplus_2").html(format_number(matrix[2][2]));
	$("#cNPV_LRplus_3").html(format_number(matrix[3][2]));
	$("#cNPV_LRplus_4").html(format_number(matrix[4][2]));

	$("#cNPV_LRminus_0").html(format_number(matrix[0][3]));
	$("#cNPV_LRminus_1").html(format_number(matrix[1][3]));
	$("#cNPV_LRminus_2").html(format_number(matrix[2][3]));
	$("#cNPV_LRminus_3").html(format_number(matrix[3][3]));
	$("#cNPV_LRminus_4").html(format_number(matrix[4][3]));



	$("#PPVcNPV_Specificity_0").html(format_number(matrix[0][0]));
	$("#PPVcNPV_Specificity_1").html(format_number(matrix[1][0]));
	$("#PPVcNPV_Specificity_2").html(format_number(matrix[2][0]));
	$("#PPVcNPV_Specificity_3").html(format_number(matrix[3][0]));
	$("#PPVcNPV_Specificity_4").html(format_number(matrix[4][0]));

	$("#PPVcNPV_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#PPVcNPV_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#PPVcNPV_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#PPVcNPV_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#PPVcNPV_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#PPVcNPV_LRplus_0").html(format_number(matrix[0][2]));
	$("#PPVcNPV_LRplus_1").html(format_number(matrix[1][2]));
	$("#PPVcNPV_LRplus_2").html(format_number(matrix[2][2]));
	$("#PPVcNPV_LRplus_3").html(format_number(matrix[3][2]));
	$("#PPVcNPV_LRplus_4").html(format_number(matrix[4][2]));

	$("#PPVcNPV_LRminus_0").html(format_number(matrix[0][3]));
	$("#PPVcNPV_LRminus_1").html(format_number(matrix[1][3]));
	$("#PPVcNPV_LRminus_2").html(format_number(matrix[2][3]));
	$("#PPVcNPV_LRminus_3").html(format_number(matrix[3][3]));
	$("#PPVcNPV_LRminus_4").html(format_number(matrix[4][3]));



	$("#ProgramBased_Specificity_0").html(format_number(matrix[0][0]));
	$("#ProgramBased_Specificity_1").html(format_number(matrix[1][0]));
	$("#ProgramBased_Specificity_2").html(format_number(matrix[2][0]));
	$("#ProgramBased_Specificity_3").html(format_number(matrix[3][0]));
	$("#ProgramBased_Specificity_4").html(format_number(matrix[4][0]));

	$("#ProgramBased_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#ProgramBased_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#ProgramBased_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#ProgramBased_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#ProgramBased_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#ProgramBased_LRplus_0").html(format_number(matrix[0][2]));
	$("#ProgramBased_LRplus_1").html(format_number(matrix[1][2]));
	$("#ProgramBased_LRplus_2").html(format_number(matrix[2][2]));
	$("#ProgramBased_LRplus_3").html(format_number(matrix[3][2]));
	$("#ProgramBased_LRplus_4").html(format_number(matrix[4][2]));

	$("#ProgramBased_LRminus_0").html(format_number(matrix[0][3]));
	$("#ProgramBased_LRminus_1").html(format_number(matrix[1][3]));
	$("#ProgramBased_LRminus_2").html(format_number(matrix[2][3]));
	$("#ProgramBased_LRminus_3").html(format_number(matrix[3][3]));
	$("#ProgramBased_LRminus_4").html(format_number(matrix[4][3]));



	$("#PPVBased_Specificity_0").html(format_number(matrix[0][0]));
	$("#PPVBased_Specificity_1").html(format_number(matrix[1][0]));
	$("#PPVBased_Specificity_2").html(format_number(matrix[2][0]));
	$("#PPVBased_Specificity_3").html(format_number(matrix[3][0]));
	$("#PPVBased_Specificity_4").html(format_number(matrix[4][0]));

	$("#PPVBased_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#PPVBased_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#PPVBased_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#PPVBased_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#PPVBased_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#PPVBased_LRplus_0").html(format_number(matrix[0][2]));
	$("#PPVBased_LRplus_1").html(format_number(matrix[1][2]));
	$("#PPVBased_LRplus_2").html(format_number(matrix[2][2]));
	$("#PPVBased_LRplus_3").html(format_number(matrix[3][2]));
	$("#PPVBased_LRplus_4").html(format_number(matrix[4][2]));

	$("#PPVBased_LRminus_0").html(format_number(matrix[0][3]));
	$("#PPVBased_LRminus_1").html(format_number(matrix[1][3]));
	$("#PPVBased_LRminus_2").html(format_number(matrix[2][3]));
	$("#PPVBased_LRminus_3").html(format_number(matrix[3][3]));
	$("#PPVBased_LRminus_4").html(format_number(matrix[4][3]));


	$("#SensitivityBased_Specificity_0").html(format_number(matrix[0][0]));
	$("#SensitivityBased_Specificity_1").html(format_number(matrix[1][0]));
	$("#SensitivityBased_Specificity_2").html(format_number(matrix[2][0]));
	$("#SensitivityBased_Specificity_3").html(format_number(matrix[3][0]));
	$("#SensitivityBased_Specificity_4").html(format_number(matrix[4][0]));

	$("#SensitivityBased_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#SensitivityBased_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#SensitivityBased_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#SensitivityBased_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#SensitivityBased_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#SensitivityBased_LRplus_0").html(format_number(matrix[0][2]));
	$("#SensitivityBased_LRplus_1").html(format_number(matrix[1][2]));
	$("#SensitivityBased_LRplus_2").html(format_number(matrix[2][2]));
	$("#SensitivityBased_LRplus_3").html(format_number(matrix[3][2]));
	$("#SensitivityBased_LRplus_4").html(format_number(matrix[4][2]));

	$("#SensitivityBased_LRminus_0").html(format_number(matrix[0][3]));
	$("#SensitivityBased_LRminus_1").html(format_number(matrix[1][3]));
	$("#SensitivityBased_LRminus_2").html(format_number(matrix[2][3]));
	$("#SensitivityBased_LRminus_3").html(format_number(matrix[3][3]));
	$("#SensitivityBased_LRminus_4").html(format_number(matrix[4][3]));


	$("#DominatedByRareDisease_Specificity_0").html(format_number(matrix[0][0]));
	$("#DominatedByRareDisease_Specificity_1").html(format_number(matrix[1][0]));
	$("#DominatedByRareDisease_Specificity_2").html(format_number(matrix[2][0]));
	$("#DominatedByRareDisease_Specificity_3").html(format_number(matrix[3][0]));
	$("#DominatedByRareDisease_Specificity_4").html(format_number(matrix[4][0]));

	$("#DominatedByRareDisease_Sensitivity_0").html(format_number(matrix[0][1]));
	$("#DominatedByRareDisease_Sensitivity_1").html(format_number(matrix[1][1]));
	$("#DominatedByRareDisease_Sensitivity_2").html(format_number(matrix[2][1]));
	$("#DominatedByRareDisease_Sensitivity_3").html(format_number(matrix[3][1]));
	$("#DominatedByRareDisease_Sensitivity_4").html(format_number(matrix[4][1]));

	$("#DominatedByRareDisease_LRplus_0").html(format_number(matrix[0][2]));
	$("#DominatedByRareDisease_LRplus_1").html(format_number(matrix[1][2]));
	$("#DominatedByRareDisease_LRplus_2").html(format_number(matrix[2][2]));
	$("#DominatedByRareDisease_LRplus_3").html(format_number(matrix[3][2]));
	$("#DominatedByRareDisease_LRplus_4").html(format_number(matrix[4][2]));

	$("#DominatedByRareDisease_LRminus_0").html(format_number(matrix[0][3]));
	$("#DominatedByRareDisease_LRminus_1").html(format_number(matrix[1][3]));
	$("#DominatedByRareDisease_LRminus_2").html(format_number(matrix[2][3]));
	$("#DominatedByRareDisease_LRminus_3").html(format_number(matrix[3][3]));
	$("#DominatedByRareDisease_LRminus_4").html(format_number(matrix[4][3]));


}
