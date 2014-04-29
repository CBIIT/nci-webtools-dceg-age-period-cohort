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

$(document).ready(function() {

    $("#calculate").click(function()
    {
        var fixedArray = ""; // prevalence
        var contourArray = ""; // ppv
        var independentArray = ""; //specificity

        var independentArray = $("#independent").val();
        independentArraySplit = independentArray.split(",");
        var independentMin = Math.min.apply( Math, independentArraySplit )
        var independentMax = Math.max.apply( Math, independentArraySplit )
        var contourArray = $("#contour").val();
        var columnHeadings = contourArray.split(",");
        var fixedArray = $("#fixed").val();
        fixedArraySplit = fixedArray.split(",");
        var fixedArraySize = fixedArraySplit.length;
    
        var fixed_dropdown = $("#fixed_dropdown").val();
        
        uniqueKey = (new Date()).getTime();
    
        var tabkey = ["Prevalence_Odds_Length"];
        var keys = ["Sensitivity_required_to_achieve_specified_PPV_given_prevalence_and_specificity"]; 
                   // "Delta_required_to_achieve_specified_PPV_given_prevalence_and_specificity"];

        var abbreviatedkeys = ["Sensitivity", 
                               "Delta"];

		open_threads = keys.length;
		error_count = 0;

		
		$("#output").empty();
		
		// First make the right tabs
		
		tabs = $("<div id='tabs' style='width:1000px;'> </div>");
		$("#output").append(tabs);
		tab_names = $("<UL> </UL>");
		tabs.append(tab_names);
		
		for (var i=0; i < fixedArraySplit.length; i++) {
			tab_names.append("<LI><a  style='padding:3px;' href='#fixed-" + (i+1) + "'>" + fixed_dropdown + ": "+ fixedArraySplit[i] + "</a></LI>");
			tab_pane = $("<DIV style='width:1000px;height:300px;' id='fixed-" + (i+1) + "' >  </div>")
			tabs.append(tab_pane);			

			table_side = ("<div class='table-side' id='table-" + (i+1) + "'></div>");
			tab_pane.append(table_side);
			graphic_side = ("<div class='graphic-side' id='graphic-" + (i+1) + "'> </div>");
			tab_pane.append(graphic_side);
		}
		tabs.tabs();

		for (var fixedValue=0; fixedValue < fixedArraySplit.length; fixedValue++)
		{
			tabindex = fixedValue+1;
			for (var keyIndex=0; keyIndex < keys.length; keyIndex++)
			{
				getData({
					key:keys[keyIndex],
					Contour:contourArray,
					reference:fixedArray,
					independent:independentArray,
					Specmin:independentMin,
					Specmax:independentMax,
					uniqueId:uniqueKey,
					tab:tabindex,
					tabvalue:fixedArraySplit[fixedValue],
					abreviatedkey:abbreviatedkeys[keyIndex]
				}, keys[keyIndex], tabindex, fixedArray[fixedValue], uniqueKey, abbreviatedkeys[keyIndex], columnHeadings);
			}
		}
     }); // calculate   
});  // ready


function getData(data, tableTitle, tabnumber, tabValue, uniqueKey, graphNamePreFix, columnHeadings) {
    hostname = window.location.hostname;
    $.ajax({
        type: "POST",
        url: "http://"+hostname+"/riskStratAdvRest/cal",
        data:data,
        dataType:"json",
        success:function(data) {
          fillTable(data, columnHeadings, tabnumber);
        },
        error: function (request, status, error) {
          handleError(error, status, request);  
        },
        complete: function(data) {
            console.log("Completing: " + tableTitle);
            open_threads--;
            if (open_threads == 0) {
               //$("#please_wait").dialog('close');
               if (error_count > 0) {
                  alert ("There were " + error_count + " errors with your request");
                  error_count=0;
               }
            }
            loadImage(tabnumber, tabValue, uniqueKey, graphNamePreFix);
        }
    });    
}

function handleError(error, status, request){

        alert(" Error is "+ error);
        alert(" Error Status is "+ status);
        alert(" Error irequest is "+ request);
}

function fillTable(jsonTableData, columnHeadings, tabnumber){
        console.log("JSON("+tabnumber+"): " + JSON.stringify(jsonTableData));

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
        headings.push({
    		sTitle: "&nbsp;"
        });
        for (var i=0;i<columnHeadings.length;i++) {
        	headings.push({
        		sTitle: columnHeadings[i]
            });
         }
        console.log("COL_HEAD: " + JSON.stringify(headings));
        
        
      var table = $("<table cellpadding='0' cellspacing='0' border='0' class='display' id='example'></table>");
      $("#table-" + tabnumber).append(table);
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
        $('#graphic-' + tabNumber).append("<img style='width: 450px ; height: 300px;' class='center' src='./img/" + graphNamePreFix + uniqueId + ".png'>");
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
