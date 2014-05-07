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
        independentArraySplit = independentArray.split(",");
        var independentMin = Math.min.apply( Math, independentArraySplit )
        var independentMax = Math.max.apply( Math, independentArraySplit )
        var contourArray = $("#contour").val();
        var columnHeadings = contourArray.split(",");
        var fixedArray = $("#fixed").val();
        var fixedArraySplit = fixedArray.split(",");
        var fixedArraySize = fixedArraySplit.length;
    
        var fixed_dropdown = $("#fixed_dropdown").val();
        
        uniqueKey = (new Date()).getTime();
    
        var tabkey = ["Prevalence_Odds_Length"];
        var keys = ["Sensitivity",
                    "Delta"];
        var titlekeys = ["Sensitivity required to achieve specified PPV given prevalence and specificity",
                    "Delta required to achieve specified PPV given prevalence and specificity"];

        var abbreviatedkeys = ["Sensitivity", 
                               "Delta"];


        var eIndependent = document.getElementById("independent_dropdown");
        var selectedIndependentValue  = eIndependent.options[eIndependent.selectedIndex].text;

        var eContour = document.getElementById("contour_dropdown");
        var selectedContourValue = eContour.options[eContour.selectedIndex].text;

	tableFsrstColLabel = selectedIndependentValue + "\\" + selectedContourValue;
		open_threads = keys.length;
		error_count = 0;

		
		$("#output").empty();
		
		// First make the right tabs
		
		tabs = $("<div id='tabs' style='width:1200px;'> </div>");
		$("#output").append(tabs);
		tab_names = $("<UL> </UL>");
		tabs.append(tab_names);
      var spacing = "<p></p><p></p><p></p>";
		
		for (var i=0; i < fixedArraySplit.length; i++) {
			tab_names.append("<LI><a  style='padding:3px;' href='#fixed-" + (i+1) + "'>" + fixed_dropdown + "<br>&nbsp&nbsp&nbsp "+ fixedArraySplit[i] + "</a></LI>");
			tab_pane = $("<DIV style='width:1100px;height:800px;' id='fixed-" + (i+1) + "' >  </div>")
			tabs.append(tab_pane);			
                        //tab_pane.append("<TABLE>");
			//table_side = ("<TR><TD><div class='table-side' id='table-" + (i+1) + "'></div></TD>");
		    for (var j=0; j < abbreviatedkeys.length; j++) {
			table_graph_div = $("<div class='set-"+ abbreviatedkeys[j] + (i+1) + "' style='width: 1100px; float: left; clear:left;'><p></p></div>");
			tab_pane.append(table_graph_div);
			graphic_side = ("<div class='graphic-side' id='graphic-" + abbreviatedkeys[j] +  (i+1) + "'><div style='clear:right;padding-top:10px;'> </div></div>");
			table_graph_div.append(graphic_side);
			table_side = $("<div class='table-side' id='table-" + abbreviatedkeys[j] + (i+1) + "'><br><h6>&nbsp;&nbsp;"+titlekeys[j]+"</h6></div><br><br>");
			table_graph_div.append(table_side);
			//graphic_side = ("<TD><div class='graphic-side' id='graphic-" + (i+1) + "'> </div></TD></TR>");
                   }
		}
                //tab_pane.append("</TABLE>");
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
				}, keys[keyIndex], tabindex, fixedArraySplit[fixedValue], uniqueKey, abbreviatedkeys[keyIndex], columnHeadings);
			}
		}
     }); // calculate   
});  // ready


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
        $('#graphic-' + graphNamePreFix + tabNumber).append("<img style='width: 650px ; height: 350px; text-align: right;' class='center' src='./img/" + graphNamePreFix + uniqueId + "-" + tabValue + ".png'>");
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
