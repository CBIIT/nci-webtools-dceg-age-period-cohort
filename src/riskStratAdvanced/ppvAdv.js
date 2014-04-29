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
        $("#please_wait").dialog({
                dialogClass: "no-titlebar",
                resizable: false,
            hide: 'highlight',
            show: 'highlight',
            width: '140px',
            autoOpen: false
        });

        $("#error_dialog").dialog({
                dialogClass: "no-titlebar",
                resizable: false,
            hide: 'highlight',
            show: 'highlight',
            width: '140px',
            autoOpen: false
        });
    $("#error_dialog").dialog("widget").position({
        my: 'center',
        at: 'center',
        of: $(this)
     });

    $("#calculate").click(function()
    {
        var fixedArray = ""; // prevalence
        var contourArray = ""; // ppv
        var independentArray = ""; //specificity

        var independentArray = $("#independent").val();
        //alert("independent array is "+independentArray);
        independentArraySplit = independentArray.split(",");
        var independentMin = Math.min.apply( Math, independentArraySplit )
        //alert("independent array min is "+independentMin);
        var independentMax = Math.max.apply( Math, independentArraySplit )
        //alert("independent array max is "+independentMax);
        var contourArray = $("#contour").val();
        var columnHeadings = contourArray.split(",");
        //alert("contour array is "+contourArray);
        var fixedArray = $("#fixed").val();
        //alert("fixed array is "+fixedArray);
        fixedArraySplit = fixedArray.split(",");
        var fixedArraySize = fixedArraySplit.length;
        //alert("fixed array size is "+fixedArraySize);
    
        uniqueKey = (new Date()).getTime();
    
        var tabkey = ["Prevalence_Odds_Length"];
        var keys = ["Sensitivity_required_to_achieve_specified_PPV_given_prevalence_and_specificity"]; 
                   // "Delta_required_to_achieve_specified_PPV_given_prevalence_and_specificity"];

        var abbreviatedkeys = ["Sensitivity", 
                               "Delta"];

		//$("#please_wait").dialog('open');
		open_threads = keys.length;
                error_count = 0;
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
              loadImage(tabnumber, tabValue, uniqueKey, graphNamePreFix);
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
            }
        });    
}

function handleError(error, status, request){

        alert(" Error is "+ error);
        alert(" Error Status is "+ status);
        alert(" Error irequest is "+ request);
}

function fillTable(jsonTableData, columnHeadings, tabnumber){
        alert(" in filltable "+ jsonTableData);
        var columnHeaderData2d = getColumnHeaderData (columnHeadings);
        $('#output' + tabnumber).dataTable( {
                "aaData": jsonTableData,
                "aoColumns": columnHeaderData2d,
                "bAutoWidth" : false,
                "bFilter": false,
                "bSearchable":false,
                "bInfo":false,
                "bPaginate": false,
                "bDestroy": true
        });
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
        $('#fixed-' + tabNumber).html("<img style='width: 75% ; height: 75%' class='center' src='./img/" + graphNamePreFix + uniqueId + ".png'>");
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
