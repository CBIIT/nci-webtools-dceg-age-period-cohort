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
var keysforfunctionnames = ["Sens", "Spec", "PPV", "cNPV", "Prev", "Delta"];
var functionnames = ["sensitivity", "specificity", "ppv", "cnpv", "prevalence", "delta"];
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
    
    if(typeof String.prototype.trim !== 'function') {
      String.prototype.trim = function() {
        return this.replace(/^\s+|\s+$/g, ''); 
      };
    }

    $("#calculate").click(function()
    {
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
     }); // calculate   
});  // ready

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
  return(rFileName)
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
        //console.log("JSON("+tabnumber+"): " + JSON.stringify(jsonTableData));

//        console.log(columnHeadings);

        
        // We have to tear apart the json return object and make a new one with the right values
        // First row is independent variable
        var independentArray = $("#independent").val();
        independentArraySplit = independentArray.split(",");
        
        
        var arr=[];
        rows = jsonTableData.length;
        for (var i=0;i<jsonTableData.length;i++) {
        	var values = [];
        	row_entries = jsonTableData[i];
        	values.push(parseFloat(independentArraySplit[i]));
        	for(var key in row_entries) {
        	    values.push(row_entries[key]);
        	}
        	arr.push(values);
        }        

        var headings = [];
        //headings.push({
    //		sTitle: "&nbsp;"
     //   });
        headings.push({
    		sTitle: tableFsrstColLabel 
        });
        for (var i=0;i<columnHeadings.length;i++) {
        	headings.push({
        		sTitle: columnHeadings[i]
            });
         }
        //console.log("COL_HEAD: " + JSON.stringify(headings));
        
        
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
        
//        $('#output' + tabnumber).dataTable( {
//                "aaData": jsonTableData,
//                "aoColumns": columnHeaderData2d,
//                "bAutoWidth" : false,
//                "bFilter": false,
//                "bSearchable":false,
//                "bInfo":false,
//                "bPaginate": false,
//                "bDestroy": true
//        });

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
