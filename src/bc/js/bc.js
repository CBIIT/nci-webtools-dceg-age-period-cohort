var oTable;
var outputTable;
var giRedraw = false;
var aData;
var numberOfRows;
var selectedData;
var referenceRow;
var refSpec;
var refSens;
var specArray;
var sensArray;
var prevArray;
var uniqueKey;

$(document).ready(function() {
	
	/* Init the table */
	//oTable = $('#inputData').dataTable( );
        oTable = $('#inputdata').dataTable ({
	        "bPaginate": false,
                "bLengthChange": false,
                "bFilter": false,
                "bSort": false,
                "bInfo": false,
                "bAutoWidth": true,
          }).makeEditable({
	//oTable = $('#inputData').dataTable( ).makeEditable({
	            sAddNewRowButtonId: "btnAddNewRow",
                    sUpdateURL: function(value, settings)
                                {
                                        return(value);
                                }
          });

        outputTable = $('#r-data').dataTable({
                "bPaginate": false,
                "bLengthChange": false,
                "bFilter": false,
                "bSort": false,
                "bInfo": false,
                "bAutoWidth": true,
                'aoColumns': [{'sWidth': '100px', 'sTitle': 'Specificity'}, 
                              {'sWidth': '100px', 'sTitle': 'Sensitivity'}, 
                              {'sWidth': '100px', 'sTitle': 'LR+'}, 
                              {'sWidth': '100px', 'sTitle': 'LR-'}, 
                              {'sWidth': '100px', 'sTitle': 'Prevalence'}, 
                              {'sWidth': '100px', 'sTitle': 'PPV'}, 
                              {'sWidth': '100px', 'sTitle': 'cNPV'}]
        });

        $( "#calculate_button" ).click(function() {
//                 get_inputs_for_calculation();
            numberOfRows = oTable.fnSettings().fnRecordsTotal(); 
    	    sensArray = "c(";
            specArray = "c(";
            prevArray = "c(";
            for (i=0;i<numberOfRows;i++)
            {
                if (i != referenceRow)
                {
                   rowData = oTable.fnGetData(i)
    	       sensArray = sensArray + rowData[0];
    	       specArray = specArray + rowData[1];
    	       prevArray = prevArray + rowData[2];
                   if (i < numberOfRows-1)
    	       {
    	          sensArray = sensArray + ",";
    	          specArray = specArray + ",";
    	          prevArray = prevArray + ",";
    	       }
                }
            }
    	    sensArray = sensArray + ")";
    	    specArray = specArray + ")";
    	    prevArray = prevArray + ")";
//                 get_data_stream();
            uniqueKey = (new Date()).getTime();
            hostname = window.location.hostname;
            $.ajax({
                    type: "POST",
                    url: "http://analysistools-dev.nci.nih.gov:8989/BiomarkerComparison",
                    data:{refSpec: refSpec, 
                           refSens: refSens,
                           specArray:specArray, 
                           sensArray: sensArray,
                           prevArray: prevArray, 
                           unique_key: uniqueKey},
                    dataType:"jsonp",
                    success:set_data,
                    error: function(jqXHR, exception) {
            		if (jqXHR.status === 0) {
                		alert('Not connect.\n Verify Network.');
            		} else if (jqXHR.status == 404) {
                		alert('Requested page not found. [404]');
            		} else if (jqXHR.status == 500) {
                		alert('Internal Server Error [500].');
            		} else if (exception === 'parsererror') {
                		alert('Requested JSON parse failed.');
            		} else if (exception === 'timeout') {
                		alert('Time out error.');
            		} else if (exception === 'abort') {
                		alert('Ajax request aborted.');
            		} else {
                		alert('Uncaught Error.\n' + jqXHR.responseText);
            		}
        	   }
            });
        });

        oTable.$('tr').click( function () {
            selectedData = oTable.fnGetData(this);
            refSpec = selectedData[1];
	    refSens = selectedData[0];
            referenceRow = oTable.fnGetPosition(this);
            setReferenceRow(referenceRow);
        } );
});

function setReferenceRow(rowIndex)
{
        numberOfRows = oTable.fnSettings().fnRecordsTotal(); 
        for (i=0;i<numberOfRows;i++)
        {
            if (i == rowIndex)
            {
                oTable.fnUpdate('true', i, 3)
	    }
	    else
	    {
                oTable.fnUpdate('false', i, 3)
	    }
	}
}

function fnClickAddRow() {
	$('#example').dataTable().fnAddData( [
		"0.1",
		"0.2",
		"0.3",
		"0.4" ] );
}


function get_inputs_for_calculation()
{
        numberOfRows = oTable.fnSettings().fnRecordsTotal(); 
	sensArray = "c(";
        specArray = "c(";
        prevArray = "c(";
        for (i=0;i<numberOfRows;i++)
        {
            if (i != referenceRow)
            {
               rowData = oTable.fnGetData(i)
	       sensArray = sensArray + rowData[0];
	       specArray = specArray + rowData[1];
	       prevArray = prevArray + rowData[2];
               if (i != numberOfRows-1)
	       {
	          sensArray = sensArray + ",";
	          specArray = specArray + ",";
	          prevArray = prevArray + ",";
	       }
            }
        }
	    sensArray = sensArray + ")";
	    specArray = specArray + ")";
	    prevArray = prevArray + ")";

        uniqueKey = (new Date()).getTime();
        hostname = window.location.hostname;
        $.ajax({
                type: "GET",
                url: "http://ncias-d1052-v.nci.nih.gov:8989/BiomarkerComparison",
                data: {refSpec: refSpec, 
                       refSens: refSens,
                       specArray:specArray, 
                       sensArray: sensArray,
                       prevArray: prevArray, 
                       unique_key: uniqueKey},
                dataType: "json",
                success: set_data,
                error: ajax_error
        });
}

function get_data_stream() {
        uniqueKey = (new Date()).getTime();
        hostname = window.location.hostname;
        $.ajax({
                type: "GET",
                url: "http://ncias-d1052-v.nci.nih.gov:8989/BiomarkerComparison",
                data: {refSpec: refSpec, 
                       refSens: refSens,
                       specArray:specArray, 
                       sensArray: sensArray,
                       prevArray: prevArray, 
                       unique_key: uniqueKey},
                dataType: "json",
                success: set_data,
                error: ajax_error
        });
}

function refreshGraph(drawgraph) {
   if (drawgraph == 1)
      {
         graph_file = "./img/"+uniqueKey+"SensSpecLR.png?";
      }
   else
      {
         graph_file = "./img/fail-message.jpg?";
      }

   d = new Date();
   $("#graph").attr("src", graph_file+d.getTime());
}

function set_data(dt)
{
        var jsonString;
        for (property in dt) {
                jsonString = dt[property];
        }
        var jsonObject = $.parseJSON(jsonString);
	//alert("jsonObject"+jsonObject);
        refreshGraph(1);
        createOutputTable(jsonObject)
}

function jsonToCell(obj)
{
      var Specificity;
      var Sensitivity;
      var LRplus;
      var LRminus;
      var Prevalence;
      var PPV;
      var cNPV;
      var value;
      for (var key in obj)
      {
          if (obj.hasOwnProperty(key))
          {
                value = obj[key];
		//alert("key  "+key+"  value  "+value);
		if (key== 'Specificity')
                { 
      		   Specificity=value;
		}
		else if (key== 'Sensitivity')
                { 
		   Sensitivity=value;
		}
		else if (key== 'LRplus')
                { 
      		   LRplus=value;
		}
		else if (key== 'LRminus')
                { 
      		   LRminus=value;
		}
		else if (key== 'Prevalence')
                { 
      		   Prevalence=value;
		}
		else if (key== 'PPV')
                { 
      		   PPV=value;
		}
		else if (key== 'cNPV')
                { 
      		   cNPV=value;
		}
          }
      }
      //alert(Specificity+" "+Sensitivity+" "+LRplus+" "+LRminus+" "+Prevalence+" "+PPV+" "+cNPV);
      outputTable.fnAddData([Specificity, Sensitivity, LRplus, LRminus, Prevalence, PPV, cNPV]);
}

function createOutputTable(jsondata)
{
      for (var each in jsondata) {
           alert(" each  "+each);
           jsonToCell(jsondata[each]);
     }
}


function ajax_error()
{
        refreshGraph(1);
   alert("ajax problem");
}
