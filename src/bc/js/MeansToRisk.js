var parameter_list = ['', 'PPV', 'cNPV', 'Specificity', 'Sensitivity', 'Delta', 'Prevalence'];
var axis_list = ['', 'Dependent (Y)', 'Independent (X)', 'Contour (Z)'];

var default_values = new Array();
var tbl_body = "";
var tbl_body_main = "";
var tab_keys  = "";
var tab_div = "";
var tbl_html = "";
var html_at = "";

var tabTemplate = "<li><a href='#{href}'>#{label}</a> <span class='ui-icon ui-icon-close' role='presentation'>Remove Tab</span></li>",
tabCounter = 0;

//default_values["PPV"] = "0.1, 0.3, 0.5, 0.7, 0.9";
default_values["PPV"] = "";
default_values["cNPV"] = "0.001, 0.005, 0.01, 0.05, 0.1";
default_values["Specificity"] = "0.8, 0.9, 0.95, 0.99, 0.995";
default_values["Sensitivity"] = "0.4, 0.55, 0.68, 0.8, 0.9";
default_values["Delta"] = "0.1, 0.2, 0.3, 0.4, 0.5";
//default_values["Prevalence"] = "0.1, 0.01, 0.001, 1e-4, 1e-5, 1e-6";
default_values["Prevalence"] = "0.5, 0.6, 0.7, 0.8, 0.9";

var fixed_values = new Array();
//fixed_values["Delta"]="2.7";
fixed_values["Delta"]="0.1, 0.3, 0.5, 0.7, 0.9";
fixed_values["Prevalence"]="0.0001";

var dependent_variable_choice;
var independent_variable_choice;
var contour_choice;
var fixed_value_choice;

var parameter_1;
var parameter_2;
var parameter_3;
var parameter_4;

$(document).ready(function() { 
 
// Before we even display the page
        var tabs = $( "#tabs" ).tabs();
	
	$( "#case_control_dialog" ).dialog({
		autoOpen: false,
		buttons: {
			"Replace": function() {
				replace_delta();
				$( this ).dialog( "close" );
			},
			"Append": function() {
				append_delta();
				$( this ).dialog( "close" );
			}

		}
	});
	
	// Set the dependent variable dropdown
 	$.each(parameter_list, function(i,v) {
	 	$(".variable_select")
			.append($("<option></option>")
			.attr("value",v)
			.text(v));  	
 	});

 	// Set all the dropdowns for the variable side
 	$.each(axis_list, function(i,v) {
	 	$(".axis_choice")
			.append($("<option></option>")
			.attr("value",v)
			.text(v));  	
 	});
 	
 	// Add Fixed Value to the Delta and Prevalence only
 	$("#Delta_choice").append($("<option></option>")
			.attr("value","Fixed Value")
			.text("Fixed Values"));
 	$("#Prevalence_choice").append($("<option></option>")
			.attr("value","Fixed Value")
			.text("Fixed Value"));

 	
 	$("#independent_variable_select").hide();
 	$("#contour_select").hide();
 	// Only show the label for fixed_value when necessary
 	$("#sl_4").hide();
 	
// Onchange events
 	
 	
	$( "#dependent_variable_select" ).change(function() {
		parameter_1 = $(this).val();
		// Reset Independent Variable Select
		$("#independent_variable_select").children().remove().end();
		
		var valid_parameters = get_valid_parameters(parameter_1, null);
 		$.each(valid_parameters, function(i,v) {
		 	$("#independent_variable_select")
			.append($("<option></option>")
			.attr("value",v)
			.text(v)); 
	 	});
 		$("#independent_variable_select").children('option[value=' + parameter_1 + ']').remove();
		$("#independent_variable_select").attr('disabled', false);
		$("#independent_variable_select").show();
 		
 		// Clear the Contour Select
		$("#contour_select").children().remove().end();
		$("#contour_select").hide();

		// Hide the whole fixed value thing
		$("#sl_4").hide();

		$(".axis_choice").val("");
		$(".axis_text").val("");
		set_axis_choice($(this).val(), axis_list[1]);
	});

	$( "#independent_variable_select" ).change(function() {
		parameter_2 = $(this).val();
		$("#contour_select").children().remove().end();
		var valid_parameters = get_valid_parameters(parameter_1, parameter_2);
	 	$.each(valid_parameters, function(i,v) {
		 	$("#contour_select")
				.append($("<option></option>")
				.attr("value",v)
				.text(v));  	
	 	});
		
		// Hide the whole fixed value thing
		$("#sl_4").hide();
	 	
		$("#contour_select").show();
	 	
		set_axis_choice($(this).val(), axis_list[2]);
	});
 
 	$( "#contour_select" ).change(function() {
		parameter_3 = $(this).val();
		var fixed_value = get_fixed_values(parameter_1, parameter_2, parameter_3);
		if (fixed_value != null) {
			$("#sl_4").show();
			$("#fixed_value_select").children().remove().end()
					.append($("<option></option>") )
					.append($("<option>" + fixed_value + "</option>") );
		} else {
			// Hide the whole fixed value thing
			$("#sl_4").hide();			
		}
		set_axis_choice($(this).val(), axis_list[3]);
	});

 	$( "#fixed_value_select" ).change(function() {
		parameter_4 = $(this).val();
		set_axis_choice($(this).val(), "Fixed Value");
	});

 	
 	$(".axis_choice").change(function() {
 		$(".axis_choice").css("background-color", "white");
		$("#dependent_variable_select").val("");
		$("#independent_variable_select").children().remove().end().hide();
		$("#contour_select").children().remove().end().hide();
		// Hide the whole fixed value option
		$("#sl_4").hide();
		
		// Dependent Variable
		if ($(this).val() == axis_list[1]) {
			// Erase old
			$("#" + parameter_1 + "_choice").val("");
			$("#" + parameter_1 + "_text").val("");

			// Check to see if this change will nullify an existing field
			if (parameter_2 == $(this).attr("name")) parameter_2 = null;
			if (parameter_3 == $(this).attr("name")) parameter_3 = null;
			if (parameter_4 == $(this).attr("name")) parameter_4 = null;
			
			parameter_1 = $(this).attr("name");
			$("#" + parameter_1 + "_text").val(default_values[parameter_1]);
		}
		
		// Independent Variable
		if ($(this).val() == axis_list[2]) {
			// Erase old
			$("#" + parameter_2 + "_choice").val("");
			$("#" + parameter_2 + "_text").val("");
			
			// Check to see if this change will nullify an existing field
			if (parameter_1 == $(this).attr("name")) parameter_1 = null;
			if (parameter_3 == $(this).attr("name")) parameter_3 = null;
			if (parameter_4 == $(this).attr("name")) parameter_4 = null;

			parameter_2 = $(this).attr("name");
			$("#" + parameter_2 + "_text").val(default_values[parameter_2]);
		}
		
		// Contour Variable
		if ($(this).val() == axis_list[3]) {
			// Erase old
			$("#" + parameter_3 + "_choice").val("");
			$("#" + parameter_3 + "_text").val("");

			// Check to see if this change will nullify an existing field
			if (parameter_1 == $(this).attr("name")) parameter_1 = null;
			if (parameter_2 == $(this).attr("name")) parameter_2 = null;
			if (parameter_4 == $(this).attr("name")) parameter_4 = null;

			parameter_3 = $(this).attr("name");
			$("#" + parameter_3 + "_text").val(default_values[parameter_3]);
		}

		// Fixed Value Variable
		if ($(this).val() == "Fixed Value") {
			// Erase old
			$("#" + parameter_4 + "_choice").val("");
			$("#" + parameter_4 + "_text").val("");

			// Check to see if this change will nullify an existing field
			if (parameter_1 == $(this).attr("name")) parameter_1 = null;
			if (parameter_2 == $(this).attr("name")) parameter_2 = null;
			if (parameter_3 == $(this).attr("name")) parameter_3 = null;

			parameter_4 = $(this).attr("name");
			$("#" + parameter_4 + "_text").val(fixed_values[parameter_4]);
		}
		
 	});

	$( "#calculate_button" ).click(function() {

		var p1_values=$("#" + parameter_1 + "_text").val();
		var p2_values=$("#" + parameter_2 + "_text").val();
		var p3_values=$("#" + parameter_3 + "_text").val();
		var p4_values=$("#" + parameter_4 + "_text").val();

		var specificity_values=($("#Specificity_text").val()).split(",");
		var specificity_outofbound = 0;
		for (var i=0; i < specificity_values.length; i++) {
    			specificity_values[i] = parseFloat(specificity_values[i], 10);
			if (specificity_values[i]> 1.0)
			{
				specificity_outofbound = 1;
			}
		}

		if (specificity_outofbound == 0)
		{
			$.ajax({
				type: "GET",
				url: "http://ncias-d1052-v.nci.nih.gov:8200/InvokeRGraph",
				data: {
					p1_name: parameter_1, 
					p2_name: parameter_2, 
					p3_name: parameter_3,
					p4_name: parameter_4,
					p1_values: p1_values,
					p2_values: p2_values,
					p3_values: p3_values,
					p4_values: p4_values
				},
				dataType: "jsonp",
				success: set_graph_data,
				error: ajax_error
			});
		}
		else
		{
                	alert("Specificity values should be below 1.0");
		}
	});
	
	$( "#case_control_button" ).click(function() {
		$("#case_control_dialog").dialog("open");
	});
});

function isEmpty(str) {
    return (!str || 0 === str.length);
}

function insideprop(events)
{
    var tbl_row = "";
    for (var prop in events) {
      if (typeof events[prop] === 'object')
      {
       	//alert(property + ':' + prop+'\n');
        if (!isEmpty(tbl_row))
           tbl_body += "<tr>"+tbl_row+"</tr>";                 
        tbl_row = ""
	var inner_object1 = events[prop];
        for (var inner_prop1 in inner_object1) {
            if (typeof inner_object1[inner_prop1] === 'object')
            {
		var inner_object2 = inner_object1[prop];
        	for (var inner_prop2 in inner_object2) {
            	if (typeof inner_object2[inner_prop2] === 'object')
            	{
       	       		alert(property + ':' + inner_prop2+'\n');
	    	}
	    	else
	    	{
               		tbl_row += "<td>"+inner_object2[inner_prop2]+"</td>";
	    	}
		}
	    }
	    else
	    {
               tbl_row += "<td>"+inner_object1[inner_prop1]+"</td>";
	    }
	}
        //insideprop(events[prop]);
      }
      else
      {
        //alert(events[prop]);
        tbl_row += "<td>"+events[prop]+"</td>";
      }
    }
    if (!isEmpty(tbl_row))
       tbl_body += "<tr>"+tbl_row+"</tr>";                 
    return tbl_body
}

function set_graph_data(dt)
{
	refreshGraph();	
}


function refreshGraph() {
   graph_file1 = "1"+"PPVSpecPrev.png?";
   graph_file2 = "2"+"PPVSpecPrev.png?";
   graph_file3 = "3"+"PPVSpecPrev.png?";
   graph_file4 = "4"+"PPVSpecPrev.png?";
   graph_file5 = "5"+"PPVSpecPrev.png?";
   d = new Date();
   $("#Delta1Graph").attr("src", graph_file1+d.getTime());
   $("#Delta2Graph").attr("src", graph_file2+d.getTime());
   $("#Delta3Graph").attr("src", graph_file3+d.getTime());
   $("#Delta4Graph").attr("src", graph_file4+d.getTime());
   $("#Delta5Graph").attr("src", graph_file5+d.getTime());
}


function set_data(dt) {
	//alert("Success");
        var jsonString;
        for (property in dt) {
                jsonString = dt[property];
        }
        alert(property + ':' + jsonString +'\n');
        //var jsonObject = $.parseJSON(jsonString);
        var jsonObject = JSON.parse(jsonString);
        //alert(property + ':' + jsonObject+'\n');

        for (property in jsonObject) {
                alert("property in json object >> "+jsonObject[property]);
        }

	var tabs = new Array();
	tabs[0] = "#Positive-Predictive-Value";
	tabs[1] = "#Complement-of-the-Negative-Predictive-Value";
	tabs[2] = "#Sensitivity";
	tabs[3] = "#Delta";
        var tabcount = 0;
	var tbl_row = "";
        tbl_body_main = "";
        tab_keys = "<ul>";
        html_at = "#tabs";
        jsonObject0 = jsonObject[0]
	for (var i = 0; i < jsonObject.length; i++) {
    		for (var prop in jsonObject[i]) {
			tab_keys = tab_keys + "<li><a href="+"\"#div"+prop+"\"><span>"+prop+"</span></a></li>";
			//tab_div = tab_div + "<div id=\"#div"+prop+"\"></div>";
    			//$(html_at).html(tab_div);
			tab_div = "";
			html_at = "\"#div"+prop+"\"";
        		alert(property + ':' + prop+'\n');
        		tbl_body_main = "<table border="+"\"2\" width="+"\"50\">";
       			if (jsonObject[i].hasOwnProperty(prop)) {
				tbl_body = ""
		     		tbl_body_main = tbl_body_main + insideprop(jsonObject[i][prop], tbl_body)
        		}
        		tbl_body_main = tbl_body_main+ "</table>";
			//tab_div = tab_div + tbl_body_main + "</div>";
    			//$(tabs[tabcount]).html(tbl_body_main);
    			addTab( tabcount, tabs[tabcount], tbl_body_main);
			tbl_body_main = ""
			tabcount = tabcount + 1;
    		}
	}
	tab_keys = tab_keys + "</ul>";
	tbl_html = tab_div;
        alert("tbl_html >> "+tbl_html);
    //$("#tabs").html(tbl_html);
    //$("#fragment2-1").html("<table border="+"i\""1"\""+">"+
    //                            "<tr> <td>graph row 1, cell 1</td> <td>graph row 1, cell 2</td> </tr> <tr> <td>graph row 2, cell 1</td> <td>graph row 2, cell 2</td> </tr> </table>");
};

function ajax_error(dt) {
	alert("Error");
};

function addTab(tabcount, tabtitle, tabcontent) {
	var label = tabtitle || "Tab " + tabcount,
	id = "tabs-" + tabcount,
	li = $( tabTemplate.replace( /#\{href\}/g, "#" + id ).replace( /#\{label\}/g, label ) ),
	tabContentHtml = tabcontent || "Tab " + tabcount + " content.";
        var tabsat = $( "#tabs" ).tabs();
	tabsat.find( ".ui-tabs-nav" ).append( li );
	tabsat.append( "<div id='" + id + "'><p>" + tabContentHtml + "</p></div>" );
	tabsat.tabs( "refresh" );
}

function set_axis_choice (id, choice) {
	// Reset the values, but only if the value is the current choice

	
	if (choice == axis_list[1]) {
		$("#" + dependent_variable_choice + "_choice").val("").css("background-color", "white"); 
		$("#" + independent_variable_choice + "_choice").val("").css("background-color", "white");
		$("#" + contour_choice + "_choice").val("").css("background-color", "white");
		$("#" + dependent_variable_choice + "_text").val("").css("background-color", "white"); 
		$("#" + independent_variable_choice + "_text").val("").css("background-color", "white");
		$("#" + contour_choice + "_text").val("").css("background-color", "white");
		$("#" + fixed_value_choice + "_choice").val("").css("background-color", "white");
		$("#" + fixed_value_choice + "_text").val("").css("background-color", "white");
		dependent_variable_choice = id;
		independent_variable_choice = null;
		contour_choice = null;
		fixed_value_choice = null;
	}
	if (choice == axis_list[2]) {
		$("#" + independent_variable_choice + "_choice").val("").css("background-color", "white");
		$("#" + contour_choice + "_choice").val("").css("background-color", "white"); 
		$("#" + independent_variable_choice + "_text").val("").css("background-color", "white");
		$("#" + contour_choice + "_text").val("").css("background-color", "white"); 
		$("#" + fixed_value_choice + "_choice").val("").css("background-color", "white");
		$("#" + fixed_value_choice + "_text").val("").css("background-color", "white");
		independent_variable_choice = id;
		contour_choice = null;
		fixed_value_choice = null;
	}
	if (choice == axis_list[3]) {
		$("#" + contour_choice + "_choice").val("").css("background-color", "white");
		$("#" + contour_choice + "_text").val("").css("background-color", "white");
		$("#" + fixed_value_choice + "_choice").val("").css("background-color", "white");
		$("#" + fixed_value_choice + "_text").val("").css("background-color", "white");
		contour_choice = id;
		fixed_value_choice = null;
	}	
	if (choice == "Fixed Value") {
		$("#" + fixed_value_choice + "_choice").val("").css("background-color", "white");
		$("#" + fixed_value_choice + "_text").val("").css("background-color", "white");
		fixed_value_choice = id;
	}
	
	$("#" + id + "_choice").val(choice).css("background-color", "yellow");


	if (choice == "Fixed Value") $("#" + id + "_text").val(fixed_values[id]);
	else $("#" + id + "_text").val(default_values[id]);
	
}

// The following is a lookup  
function get_valid_parameters (p1, p2){

	if (p1 == 'PPV' && p2== null) valid_parameters = ['', 'Specificity', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'cNPV' && p2 == null) valid_parameters = ['', 'Specificity', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'Specificity' && p2 == null) valid_parameters = ['', 'PPV', 'cNPV', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'Sensitivity' && p2 == null) valid_parameters = ['', 'PPV', 'cNPV', 'Specificity', 'Delta', 'Prevalence'];
	if (p1 == 'Delta' && p2 == null) valid_parameters = ['', 'PPV', 'cNPV', 'Specificity', 'Sensitivity', 'Prevalence'];
	if (p1 == 'Prevalence' && p2 == null) valid_parameters = ['', 'PPV', 'cNPV', 'Specificity', 'Sensitivity', 'Delta'];

	if (p1 == 'Specificity' && p2 == "PPV") valid_parameters = ['', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'Sensitivity' && p2 == "PPV") valid_parameters = ['', 'Specificity', 'Delta', 'Prevalence'];
	if (p1 == 'Delta' && p2 == "PPV") valid_parameters = ['', 'Specificity', 'Sensitivity'];
	if (p1 == 'Prevalence' && p2 == "PPV") valid_parameters = ['', 'Specificity', 'Sensitivity'];

	if (p1 == 'Specificity' && p2 == "cNPV") valid_parameters = ['', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'Sensitivity' && p2 == "cNPV") valid_parameters = ['', 'Specificity', 'Delta', 'Prevalence'];
	if (p1 == 'Delta' && p2 == "cNPV") valid_parameters = ['', 'Specificity', 'Sensitivity'];
	if (p1 == 'Prevalence' && p2 == "cNPV") valid_parameters = ['', 'Specificity', 'Sensitivity'];

	if (p1 == 'PPV' && p2 == "Specificity") valid_parameters = ['', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'cNPV' && p2 == "Specificity") valid_parameters = ['', 'Sensitivity', 'Delta', 'Prevalence'];
	if (p1 == 'Sensitivity' && p2 == "Specificity") valid_parameters = ['', 'PPV', 'cNPV', 'Prevalence'];
	if (p1 == 'Delta' && p2 == "Specificity") valid_parameters = ['', 'PPV', 'cNPV', 'Prevalence'];
	if (p1 == 'Prevalence' && p2 == "Specificity") valid_parameters = ['', 'PPV', 'cNPV', 'Delta'];
	
	if (p1 == 'PPV' && p2== 'Sensitivity') valid_parameters = ['', 'Specificity', 'Delta', 'Prevalence'];
	if (p1 == 'cNPV' && p2 == 'Sensitivity') valid_parameters = ['', 'Specificity', 'Delta', 'Prevalence'];
	if (p1 == 'Specificity' && p2 == 'Sensitivity') valid_parameters = ['', 'PPV', 'cNPV', 'Prevalence'];
	if (p1 == 'Delta' && p2 == 'Sensitivity') valid_parameters = ['', 'PPV', 'cNPV', 'Prevalence'];
	if (p1 == 'Prevalence' && p2 == 'Sensitivity') valid_parameters = ['', 'PPV', 'cNPV', 'Delta'];

	if (p1 == 'PPV' && p2== 'Delta') valid_parameters = ['', 'Specificity', 'Sensitivity'];
	if (p1 == 'cNPV' && p2 == 'Delta') valid_parameters = ['', 'Specificity', 'Sensitivity'];
	if (p1 == 'Specificity' && p2 == 'Delta') valid_parameters = ['', 'PPV', 'cNPV', 'Prevalence'];
	if (p1 == 'Sensitivity' && p2 == 'Delta') valid_parameters = ['', 'PPV', 'cNPV','Prevalence'];
	if (p1 == 'Prevalence' && p2 == 'Delta') valid_parameters = ['', 'Specificity', 'Sensitivity'];

	if (p1 == 'PPV' && p2== 'Prevalence') valid_parameters = ['', 'Specificity', 'Sensitivity'];
	if (p1 == 'cNPV' && p2 == 'Prevalence') valid_parameters = ['', 'Specificity', 'Sensitivity'];
	if (p1 == 'Specificity' && p2 == 'Prevalence') valid_parameters = ['', 'PPV', 'cNPV', 'Delta'];
	if (p1 == 'Sensitivity' && p2 == 'Prevalence') valid_parameters = ['', 'PPV', 'cNPV', 'Delta'];
	if (p1 == 'Delta' && p2 == 'Prevalence') valid_parameters = ['', 'Specificity', 'Sensitivity'];

	
	return valid_parameters;
}

function get_fixed_values(p1, p2, p3) {
	// There is probably a better way to do this that involves sorting by name 
	// and then checking each combo once, but logic requires too much thought right now
	
// Must check all the combos for PPV, Sens, Spec
	if (p1 == 'PPV' && p2== 'Sensitivity' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'PPV' && p2== 'Specificity' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'PPV' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'Specificity' && p3=='PPV') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'PPV' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'Sensitivity' && p3=='PPV') return 'Prevalence';

// Must check all the combos for cNPV, Sens, Spec
	if (p1 == 'cNPV' && p2== 'Sensitivity' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'cNPV' && p2== 'Specificity' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'cNPV' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'Specificity' && p3=='cNPV') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'cNPV' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'Sensitivity' && p3=='cNPV') return 'Prevalence';

	
// Must check all the combos for PPV, Sens, Delta
	if (p1 == 'PPV' && p2== 'Sensitivity' && p3=='Delta') return 'Prevalence';
	if (p1 == 'PPV' && p2== 'Delta' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'PPV' && p3=='Delta') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'Delta' && p3=='PPV') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'PPV' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'Sensitivity' && p3=='PPV') return 'Prevalence';

// Must check all the combos for cNPV, Sens, Delta
	if (p1 == 'cNPV' && p2== 'Sensitivity' && p3=='Delta') return 'Prevalence';
	if (p1 == 'cNPV' && p2== 'Delta' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'cNPV' && p3=='Delta') return 'Prevalence';
	if (p1 == 'Sensitivity' && p2== 'Delta' && p3=='cNPV') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'cNPV' && p3=='Sensitivity') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'Sensitivity' && p3=='cNPV') return 'Prevalence';

// Must check all the combos for PPV, Delta, Spec
	if (p1 == 'PPV' && p2== 'Delta' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'PPV' && p2== 'Specificity' && p3=='Delta') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'PPV' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'Specificity' && p3=='PPV') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'PPV' && p3=='Delta') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'Delta' && p3=='PPV') return 'Prevalence';

// Must check all the combos for cNPV, Delta, Spec
	if (p1 == 'cNPV' && p2== 'Delta' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'cNPV' && p2== 'Specificity' && p3=='Delta') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'cNPV' && p3=='Specificity') return 'Prevalence';
	if (p1 == 'Delta' && p2== 'Specificity' && p3=='cNPV') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'cNPV' && p3=='Delta') return 'Prevalence';
	if (p1 == 'Specificity' && p2== 'Delta' && p3=='cNPV') return 'Prevalence';

// Now for the fixed delta 	

	// Must check all the combos for PPV, Sens, Prev
	if (p1 == 'PPV' && p2== 'Sensitivity' && p3=='Prevalence') return 'Delta';
	if (p1 == 'PPV' && p2== 'Prevalence' && p3=='Sensitivity') return 'Delta';
	if (p1 == 'Sensitivity' && p2== 'PPV' && p3=='Prevalence') return 'Delta';
	if (p1 == 'Sensitivity' && p2== 'Prevalence' && p3=='PPV') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'PPV' && p3=='Sensitivity') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'Sensitivity' && p3=='PPV') return 'Delta';

// Must check all the combos for cNPV, Sens, Prev
	if (p1 == 'cNPV' && p2== 'Sensitivity' && p3=='Prevalence') return 'Delta';
	if (p1 == 'cNPV' && p2== 'Prevalence' && p3=='Sensitivity') return 'Delta';
	if (p1 == 'Sensitivity' && p2== 'cNPV' && p3=='Prevalence') return 'Delta';
	if (p1 == 'Sensitivity' && p2== 'Prevalence' && p3=='cNPV') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'cNPV' && p3=='Sensitivity') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'Sensitivity' && p3=='cNPV') return 'Delta';

// Must check all the combos for PPV, Prev, Spec
	if (p1 == 'PPV' && p2== 'Prevalence' && p3=='Specificity') return 'Delta';
	if (p1 == 'PPV' && p2== 'Specificity' && p3=='Prevalence') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'PPV' && p3=='Specificity') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'Specificity' && p3=='PPV') return 'Delta';
	if (p1 == 'Specificity' && p2== 'PPV' && p3=='Prevalence') return 'Delta';
	if (p1 == 'Specificity' && p2== 'Prevalence' && p3=='PPV') return 'Delta';

// Must check all the combos for cNPV, Prevalence, Spec
	if (p1 == 'cNPV' && p2== 'Prevalence' && p3=='Specificity') return 'Delta';
	if (p1 == 'cNPV' && p2== 'Specificity' && p3=='Prevalence') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'cNPV' && p3=='Specificity') return 'Delta';
	if (p1 == 'Prevalence' && p2== 'Specificity' && p3=='cNPV') return 'Delta';
	if (p1 == 'Specificity' && p2== 'cNPV' && p3=='Prevalence') return 'Delta';
	if (p1 == 'Specificity' && p2== 'Prevalence' && p3=='cNPV') return 'Delta';
}

function replace_delta() {
	var delta = calculate_delta();
	
	$("#Delta_text").attr("value", "" + delta.toPrecision(4));

}

function append_delta() {
	var delta = calculate_delta();
	
	var old_text = $("#Delta_text").attr("value");
	
	$("#Delta_text").attr("value", old_text + " " + delta.toPrecision(4));
	
}

function calculate_delta() {
	var mean_cases = parseFloat($("#mean_cases_input").val());
	var mean_controls = parseFloat($("#mean_controls_input").val());
	var stderr_cases = parseFloat($("#stderr_cases_input").val());
	var stderr_controls = parseFloat($("#stderr_controls_input").val());
	var N_cases = parseFloat($("#N_cases_input").val());
	var N_controls = parseFloat($("#N_controls_input").val());

// Now calculate Delta	
	var deviation_cases= stderr_cases * Math.sqrt(N_cases);
	var deviation_controls= stderr_controls * Math.sqrt(N_controls);
	var variance_cases= deviation_cases * deviation_cases;	
	var variance_controls= deviation_controls * deviation_controls;	
	var variance_overall = ( (N_cases * variance_cases) + (N_controls * variance_controls) )/ (N_cases + N_controls);
	
	var mean_overall = ( (N_cases * mean_cases) + (N_controls * mean_controls) )/ (N_cases + N_controls);
	var N_overall = N_cases + N_controls;

	var cv_cases = Math.sqrt(variance_cases) / mean_cases;
	var cv_controls = Math.sqrt(variance_controls) / mean_controls;
	var cv_overall = Math.sqrt(variance_overall) / mean_overall;
	
	var difference_in_mean = mean_cases - mean_controls;
	
	var delta = difference_in_mean / Math.sqrt(variance_overall);

	return delta;
}
