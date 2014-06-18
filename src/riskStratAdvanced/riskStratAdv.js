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
var tableFirstColLabel;
var keysforfunctionnames = ["", "Sens", "Spec", "PPV", "cNPV", "Prev", "Delta"];

var functionnames = ["", "sensitivity", "specificity", "ppv", "cnpv", "prevalence", "delta"];
var invalidCombos =    ["delta-sensitivity-specificity",
						"cnpv-delta-ppv",
						"cnpv-ppv-prevalence",
						"cnpv-ppv-sensitivity",
						"cnpv-ppv-specificity"];
var initialData = ["", 
                   "e.g. 0.8, 0.85,0.9, 0.95, 0.995", 
                   "e.g. 0.6,0.75,0.8,0.86,0.92",
                    "e.g. 0.6,0.7,0.8,0.9,0.95",
                    "e.g. 0.39,0.48,0.59,0.62,0.78",
                    "e.g. 0.1,0.2,0.3,0.4,0.5",
                    "e.g. 1,2,3,4,5"];
var	activeSelectionChange = false;
var validCombo = false;

var keysforfunction = [{1:"Sens"}, {2:"Spec"}, {3:"PPV"}, {4:"cNPV"}, {5:"Prev"}, {6:"Delta"}];
var keysforfunction = [{1:"sensitivity"}, {2:"specificity"}, {3:"ppv"}, {4:"cnpv"}, {5:"prevalence"}, {6:"delta"}];

var rfunctions = ["SensPPVDelta", 
                  "SensPPVPrev", 
                  "SensPPVSpec",
		  "SensPrevDelta",
		  "SenscNPVDelta",
		  "SenscNPVPrev",
		  "SenscNPVSpec",
                  "SpecPPVDelta",
                  "SpecPPVPrev", 
		  "SpecPrevDelta",
		  "SpeccNPVDelta",
                  "SpeccNPVPrev"]; 
var keyShort = [{1:"Prevalence"}, 
            {1:'Delta', 2:'Specificity'}, 
	    {1:'Prevalence', 2:'Delta'},
	    {1:'PPV', 2:'cNPV'},
	    {1:'Prevalence'},
	    {1:'Delta', 2:'Specificity'},
	    {1:'Prevalence', 2:'Delta'},
            {1:'Prevalence', 2:'Sensitivity'},
            {1:'Delta', 2:'Sensitivity'}, 
	    {1:'PPV', 2:'cNPV'},
            {1:'Prevalence'},
            {1:'Delta', 2:'Sensitivity'}]; 
var keyLong = [{1:"Prevalence required to achieve specified PPV given delta and sensitivity", 2:"Prevalence required to achieve specified PPV given delta and sensitivity"},
{1:"Delta required to achieve specified PPV given prevalence and sensitivity", 2:"Specificity required to achieve specified PPV given prevalence and sensitivity"}, 
{1:"Prevalence required to achieve specified PPV given specificity and sensitivity", 2:"Delta required to achieve specified PPV given specificity and sensitivity"},
{1:"Delta required to achieve specified PPV given specificity and sensitivity", 2:"Complement of the Negative Predictive Value given sensitivity, prevalence, and delta", 3:"Specificity given delta and sensitivity"},
{1:"(Prevalence required to achieve specified cNPV given delta and sensitivity", 2:"Specificity required to achieve specified cNPV given delta and sensitivity"},
{1:"Delta required to achieve specified cNPV given prevalence and sensitivity", 2:"Specificity required to achieve specified cNPV given prevalence and sensitivity"},
{1:"Prevalence required to achieve specified cNPV given specificity and sensitivity", 2:"Delta given specificity and sensitivity"},
{1:"Prevalence required to achieve specified PPV given delta and specificity", 2:"Sensitivity required to achieve specified PPV given delta and specificity"},
{1:"Delta required to achieve specified PPV given prevalence and specificity", 2:"Sensitivity required to achieve specified PPV given prevalence and specificity"},
{1:"Positive Predictive Value given specificity, prevalence, and delta", 2:"Complement of the Negative Predictive Value given specificity, prevalence, and delta", 3:"Sensitivity given delta and specificity"},
{1:"Prevalence required to achieve specified cNPV given delta and specificity", 2:"Sensitivity required to achieve specified cNPV given delta and specificity"},
{1:"Delta required to achieve specified cNPV given prevalence and specificity", 2:"Sensitivity required to achieve specified cNPV given prevalence and specificity"}];

$(document).ready(function() {
	//Create a dialog box to ask user if they would like to continue on rule violation.
	 $(function() {
		 $( "#dialog-confirm" ).dialog({
			 resizable: false,
			 height:375,
			 width: 400,
			 autoOpen: false,
			 modal: true,
			 buttons: {
				 Yes: function() {
					 $( this ).dialog( "close" );
					// alert("calculate");
					 calculate();
				 },
				 Cancel: function() {
					 $( this ).dialog( "close" );
					// alert('Cancel');
				 }
			 }
		 });
	});
	/* 
    $("#test-rules" ).button().click(function() {
   		$( "#dialog-confirm" ).dialog("open");
   		return false;
    });
	*/

	$( "select" ).change(function() {
			makeSelectionsUnique(functionnames, this.id);
	});
    
    if(typeof String.prototype.trim !== 'function') {
      String.prototype.trim = function() {
        return this.replace(/^\s+|\s+$/g, ''); 
      };
    }
    $("#reset").button().click(function() {
		makeSelectionsUnique(functionnames, "independent_dropdown");
    	$("span.variable-example").text("");
    	$("option").removeAttr("disabled");
		$("#status-bar").css("visibility", "hidden");
    });
    $("input").keyup(function(){
    	console.log("keyup detected");
    	var selectedValues = [];
    	//Get ids from select elements
    	var ids = $("input").map(function() {
        	return this.id;
    	}).get();
    	//Save currently selected values
    	$.each( ids, function( key, elementId ) {
    		selectedValues.push($('#'+ elementId).val().length);
    	});
    	console.log('selectedValues');
    	console.dir(selectedValues);
    	console.dir(validCombo);
    	if($.inArray(0, selectedValues) == -1 && validCombo) {
    		console.log("ENABLE BUTTON");
    		$( "#calculate" ).button( "option", "disabled", false );
    	} else {
    		console.log("DISABLE BUTTON");
    		$( "#calculate" ).button( "option", "disabled", true );
    	}
    });
    $("#calculate").button().click(function() {
    	if(checkRules() == "Fail") {
       		$( "#dialog-confirm" ).dialog("open");
       		return false;
    	} else {
        	//alert("calculate");
        	calculate();
    	}
    }); // calculate   
});  // ready

function checkRules() {
	
	var overallStatus = "Pass";
	var numberOfRules = 5;
	var vars = [];
	var values = [];
	$(".rule").removeAttr("style");
	//get vars
	//get values
	for(var ruleId=1; ruleId<=numberOfRules; ruleId++) {
		if(checkRule(ruleId, vars, values) == "Fail"){
			ruleClass = "rule"+ruleId;
			$("."+ruleClass).css("font-weight", "bold");
			overallStatus = "Fail";
		}
	}
	
	overallStatus = "Fail";
	console.log("overallStatus = "+overallStatus);
	return overallStatus;

}

function checkRule(ruleId, vars, values) {
	console.info("checking rule "+ruleId);
	staus = "Pass";
	switch(ruleId) {
	case 1:
		//
		//Rule 1:
		//Specificity, Sensitivity, PPV, cNPV, Prevalence can only be 0 to 1
		//
		status = "Pass";
		break;
	case 2:
		//
		//Rule 2:
		//Delta can be 0 to 5
		//
		status = "Pass";
	    break;
	case 3:
		//
		//Rule 3:
		//cNPV<PPV ...	For arrays: max(cNPV)<min(PPV)
		//
		status = "Pass";
	    break;
	case 4:
		//
		//Rule 4:
		// cNPV < Prevalence < PPV... For arrays: max(prev)<min(PPV)  and max(cNPV)<min(Prevalence)
		//
		status = "Fail";
	    break;
	case 5:
		//
		//Rule 5:
		//Sensivitity+Specificity-1>0
		//
		status = "Pass";
	    break;
	}
	
	return status;
}
function calculate() {
    var fixedArray = ""; // prevalence
    var contourArray = ""; // ppv
    var independentArray = ""; //specificity

    var independentArray = $("#independent").val();
    var independentval = $("#independent_dropdown").val();
    independentArraySplit = independentArray.split(",");
    var independentMin = Math.min.apply( Math, independentArraySplit )
    var independentMax = Math.max.apply( Math, independentArraySplit )
    var contourArray = $("#contour").val();
    var contourval = $("#contour_dropdown").val();
    var columnHeadings = contourArray.split(",");
    var fixedArray = $("#fixed").val();
    var fixedval = $("#fixed_dropdown").val();
    var fixedArraySplit = fixedArray.split(",");
    var fixedArraySize = fixedArraySplit.length;

    var fixed_dropdown = $("#fixed_dropdown").val();
    
    uniqueKey = (new Date()).getTime();

    var tabkey = ["Prevalence_Odds_Length"];
    //var keys = ["Sensitivity",
    //            "Delta"];
    var titlekeys = ["Sensitivity required to achieve specified PPV given prevalence and specificity",
                "Delta required to achieve specified PPV given prevalence and specificity"];

    var abbreviatedkeys = ["Sensitivity", 
                           "Delta"];
    var numberOfKeysForCurrentFunction = 0;

    var keyvalueIndex = getKeyValueIndex(independentval, fixedval, contourval);
    if (keyvalueIndex >= 0)
    {
    	var keyvalueShort = keyShort[keyvalueIndex];
    	var keyvalueLong  = keyLong[keyvalueIndex];   
	for (var key in keyvalueShort) {
		numberOfKeysForCurrentFunction++;
	}		
    	var eIndependent = document.getElementById("independent_dropdown");
    	var selectedIndependentValue  = eIndependent.options[eIndependent.selectedIndex].text;

    	var eContour = document.getElementById("contour_dropdown");
    	var selectedContourValue = eContour.options[eContour.selectedIndex].text;

	tableFsrstColLabel = selectedIndependentValue + "\\" + selectedContourValue;
	open_threads = numberOfKeysForCurrentFunction.length;
	error_count = 0;

	
	$("#output").empty();
	
	// First make the right tabs
	
	tabs = $("<div id='tabs' style='width:1500px;'> </div>");
	$("#output").append(tabs);
	tab_names = $("<UL> </UL>");
	tabs.append(tab_names);
  		var spacing = "<p></p><p></p><p></p>";
	
	for (var i=0; i < fixedArraySplit.length; i++) {
		tab_names.append("<LI><a  style='padding:3px;' href='#fixed-" + (i+1) + "'>" + fixed_dropdown + "<br>&nbsp&nbsp&nbsp "+ fixedArraySplit[i] + "</a></LI>");
		tab_pane = $("<DIV style='width:1500px;height:1100px;' id='fixed-" + (i+1) + "' >  </div>")
		tabs.append(tab_pane);			
                    //tab_pane.append("<TABLE>");
		//table_side = ("<TR><TD><div class='table-side' id='table-" + (i+1) + "'></div></TD>");
	    //for (var j=0; j < abbreviatedkeys.length; j++) {
	    for (var key in keyvalueShort) {
		//table_graph_div = $("<div class='set-"+ abbreviatedkeys[j] + (i+1) + "' style='width: 1100px; float: left; clear:left;'><p></p></div>");
		table_graph_div = $("<div class='set-"+ keyvalueShort[key] + (i+1) + "' style='width: 1200px; float: left; clear:left;'><p></p></div>");
		tab_pane.append(table_graph_div);
		graphic_side = ("<div class='graphic-side' id='graphic-" + keyvalueShort[key] +  (i+1) + "'><div style='clear:right;padding-top:10px;'> </div></div>");
		table_graph_div.append(graphic_side);
		table_side = $("<div class='table-side' id='table-" + keyvalueShort[key] + (i+1) + "'><br><h5>&nbsp;&nbsp;"+keyvalueLong[key]+"</h5></div><br><br>");
		table_graph_div.append(table_side);
		//graphic_side = ("<TD><div class='graphic-side' id='graphic-" + (i+1) + "'> </div></TD></TR>");
               }
	}
            //tab_pane.append("</TABLE>");
	tabs.tabs();

	for (var fixedValue=0; fixedValue < fixedArraySplit.length; fixedValue++)
	{
		tabindex = fixedValue+1;
		//for (var keyIndex=0; keyIndex < keys.length; keyIndex++)
		for (var shortkey in keyvalueShort)
		{
			getData({
				key:keyvalueShort[shortkey],
				keyindex:shortkey,
				independentval:independentval,
				fixedval:fixedval,
				contourval:contourval,
				independent:independentArray,
				fixed:fixedArray,
				Contour:contourArray,
				Specmin:independentMin,
				Specmax:independentMax,
				uniqueId:uniqueKey,
				tab:tabindex,
				tabvalue:fixedArraySplit[fixedValue],
				abreviatedkey:keyvalueShort[shortkey]
			}, keyvalueShort[shortkey], tabindex, fixedArraySplit[fixedValue], uniqueKey, keyvalueShort[shortkey], columnHeadings);
		}
	}
} // if function mapping is available
    else
    {
            $("#output").empty();
    }
}	

	
function getKeyValueIndex(independentvalue, fixedvalue, contourvalue) {

  rfunctionname = getFunctionName(independentvalue, fixedvalue, contourvalue);
  //alert(rfunctionname);

  for (var functions=0; functions < rfunctions.length; functions++)
  {
     if (rfunctions[functions] == rfunctionname)
        return functions;
  }
  alert("no function mapping available");
  return -1;
}

function getFunctionName(independent, fixed, contour) {
  rFileName = "";
  var inputnames = [];
  inputnames[0] = independent;
  inputnames[1] = fixed;
  inputnames[2] = contour;
  for (var name=0;  name < functionnames.length; name++)
  {
    for (var variablename=0; variablename<inputnames.length; variablename++)
    {
      if (functionnames[name] == inputnames[variablename])
      {
        rFileName = rFileName.concat(keysforfunctionnames[name]);
      }
    }
  }
  return(rFileName);
}


function getData(data, tableTitle, tabnumber, tabValue, uniqueKey, abbreviatedKey,  columnHeadings) {
    hostname = window.location.hostname;
    $.ajax({
        type: "POST",
        url: "http://"+hostname+"/riskStratAdvRest/cal",
        data:data,
        dataType:"json",
        success:function(data) {
          fillTable(data, columnHeadings, tabnumber, abbreviatedKey);
        },
        error: function (request, status, error) {
          handleError(error, status, request);  
        },
        complete: function(data) {
            //console.log("Completing: " + tableTitle);
            open_threads--;
            if (open_threads == 0) {
               //$("#please_wait").dialog('close');
               if (error_count > 0) {
                  alert ("There were " + error_count + " errors with your request");
                  error_count=0;
               }
            }
            loadImage(tabnumber, tabValue.trim(), uniqueKey, abbreviatedKey);
            //fillTable(data, columnHeadings, tabnumber, abbreviatedKey);
        }
    });    
}

function handleError(error, status, request){

        alert(" Error is "+ error);
        alert(" Error Status is "+ status);
        alert(" Error irequest is "+ request);
}

function fillTable(jsonTableData, columnHeadings, tabnumber, abbreviatedKey){
        var independentArray = $("#independent").val();
        independentArraySplit = independentArray.split(",");


        var arr=[];
        var tableData = jsonTableData[0].data;
        var tableError = jsonTableData[0].error;
        if (tableError[0].true != 1)
        {
                rows = tableData.length;
                for (var i=0;i<tableData.length;i++) {
                        var values = [];
                        row_entries = tableData[i];
                        values.push(parseFloat(independentArraySplit[i]));
                        for(var key in row_entries) {
                                values.push(row_entries[key]);
                        }
                        arr.push(values);
                }

                var headings = [];
                headings.push({
                        sTitle: tableFsrstColLabel
                });
                for (var i=0;i<columnHeadings.length;i++) {
                        headings.push({
                                sTitle: columnHeadings[i]
                        });
                }


                var table = $("<table cellpadding='0' cellspacing='0' border='0' class='display' id='example'></table>");
                $("#table-" + abbreviatedKey + tabnumber).append(table);
                table.dataTable( {
                        "aaData": arr,
                        "aoColumns": headings,
                        "bAutoWidth" : false,
                        "bFilter": false,
                        "bSearchable":false,
                        "bInfo":false,
                        "bSort":false,
                        "bPaginate": false,
                        "bDestroy": true,
                        "aaSorting": [[ 0, "asc" ]]
                });
        }
        else
        {
                $("#status-bar").append(tableError[1].message);
        }
}

function getColumnHeaderData(columnHeadings) {
        var columnHeaderData2d = new Array();
        for (var key in columnHeadings){
                var tempObject = {};
                tempObject["mDataProp"] = columnHeadings[key];
                tempObject["sTitle"] = columnHeadings[key];
                tempObject["sWidth"] = "25%";
                columnHeaderData2d.push(tempObject);
        }
        return columnHeaderData2d;
}


function loadImage(tabNumber, tabValue, uniqueId, graphNamePreFix) {
        $('#graphic-' + graphNamePreFix + tabNumber).append("<img style='height: 400px; text-align: right;' class='center' src='./img/" + graphNamePreFix + uniqueId + "-" + tabValue + ".png'>");
}


function isNumberBetweenZeroAndOne(n) {
	if (isNaN(parseFloat(n))) return false;
	if (n > 1) return false;
	if (n < 0) return false;
	return true;
}

function refreshGraph(drawgraph) {
   if (drawgraph == 1) graph_file = "./tmp/"+uniqueKey+"SensSpecLR.jpg?";
   else graph_file = "./images/fail-message.jpg?";

   d = new Date();
   $("#graph").attr("src", graph_file+d.getTime());
}

function ajax_error(jqXHR, exception)
{	
   refreshGraph(1);
   alert("ajax problem");
}

function makeSelectionsUnique(originalOptions, elementId) {
	
	var selectedValues = [];
	var disabledValues = [];
	
	$( "#calculate" ).button( "option", "disabled", true );

	if(activeSelectionChange == true)
		return;
	
	activeSelectionChange = true;
	
	//Get ids from select elements
	var ids = $("select").map(function() {
    	return this.id;
	}).get();
	
	//Save currently selected values
	$.each( ids, function( key, elementId ) {
		selectedValues.push($('#'+ elementId+' option:selected').val());
	});
	
	//Repopulate each dropdown with the original list and reselect.
	for (var key = 0; key < ids.length; key++) {
		disabledValues = [];
		for(var i = 0; i < selectedValues.length; i++) {
			if(i != key  && selectedValues[i] != "") {
				disabledValues.push(selectedValues[i]);
			}
		}
		
		dropdownBoxId = ids[key];
		removeAllOptions(dropdownBoxId);
		addAllOptions(dropdownBoxId, originalOptions, disabledValues);
		
		//Reselect User selection
		$('#'+dropdownBoxId).val(selectedValues[key]).change();
	}
	//If sandbox populate with default values
	//if(window.location.hostname == "analysistools-sandbox.nci.nih.gov" ||
	//		window.location.hostname == "localhost")
	setInitialValue(elementId);
	checkForInvalidVariableCombo(elementId);
	activeSelectionChange = false;
	
}

function removeAllOptions(eid) {
	element = document.getElementById(eid);
    var i;
    for(i=element.options.length-1;i>=0;i--) {
    	element.remove(i);
    }
}

function addAllOptions(dropdownBoxId, originalOptions, disabledOptions) {
	for( var optionKey = 0; optionKey < originalOptions.length; optionKey++) {
		if($.inArray(originalOptions[optionKey], disabledOptions) > -1) {
			attribute = 
				$('#'+dropdownBoxId)
	    			.append($("<option></option>")
	        		.attr("value",originalOptions[optionKey])
	        		.attr('disabled','disabled')
	        		.text(originalOptions[optionKey])); 
        } else {
			attribute = 
				$('#'+dropdownBoxId)
	    			.append($("<option></option>")
	        		.attr("value",originalOptions[optionKey])
	        		.text(originalOptions[optionKey])); 
        }
	}
}

function checkForValidVariablesSelection() {
	//console.log("chekForValidVariablesSelection()");
	//alert("chekForValidVariablesSelection()");
}

function checkForValidRange() {
	//alert("chekForValidRange()");
	
}

function setInitialValue(textboxId) {
	
	selectedOption = $("#"+textboxId+" option:selected" ).val();
	key = $.inArray(selectedOption, functionnames);
	
	eSelect = document.getElementById(textboxId);
	//Get the parent row <tr> of this <select>
	eSelect2 = $(eSelect).parent().parent()[0];

	//This next command removes the selected attribute from options, 
	//so we will reselect it later.
	$(eSelect2).find(":input").val("");
	$(eSelect2).find("span").text(initialData[key]);

	//Reselect User selection
	$('#'+textboxId).val(selectedOption).change();
}

function checkForInvalidVariableCombo() {
	//Get array of variables

	//Get ids from select elements
	var selectedValues = [];
	var ids = $("select").map(function() {
    	return this.id;
	}).get();
	
	//Save currently selected values
	$.each( ids, function( key, elementId ) {
		selectedValues.push($('#'+ elementId+' option:selected').val());
	});
	
	//Make sure all three variables exists else return
	blankCount = $.inArray("", selectedValues);
	if($.inArray("", selectedValues) == -1) {
		//All three variables are slected.  Check if it is valid.
		selectedValuesSorted = selectedValues.sort(); 
		selectedValuesSortedString = selectedValues.join("-");
		
		if($.inArray(selectedValuesSortedString, invalidCombos) >= 0) {
			//INVALID COMBO FOUND
			userSelectedVariables = selectedValues[0].toString() + ", " +
								selectedValues[1].toString() + ",  and " +
								selectedValues[2].toString();
			message = "The variables " + userSelectedVariables + 
					" do not form a valid variable combination for this calculation.  " + 
					"Please select a vaild variable combination.";
			$("#status-bar").css("visibility", "visible");
			$("#status-bar").addClass("status-error");
			$("#status-bar").removeClass("status-info");
			$("#status-bar").text(message);
			//$( "#calculate" ).attr("disabled", "disabled");
			//$( "#calculate" ).button( "option", "disabled", true );
			validCombo = false;
		} else {
			//VALID COMBO FOUND
			$("#status-bar").css("visibility", "hidden");
			$("#status-bar").addClass("status-error");
			$("#status-bar").removeClass("status-info");
			$("#status-bar").text("VALID COMBO FOUND.");
			//$( "#calculate" ).removeAttr("disabled");
			//$( "#calculate" ).button( "option", "disabled", false );
			validCombo = true;
		}
	} else {
		$("#status-bar").css("visibility", "hidden");
		$("#status-bar").addClass("status-info");
		$("#status-bar").removeClass("status-error");
		$("#status-bar").text("You have unselected values.");
		//$( "#calculate" ).attr("disabled", "disabled");
		//$( "#calculate" ).button( "option", "disabled", true );
		validCombo = false;

		return
	}

}
