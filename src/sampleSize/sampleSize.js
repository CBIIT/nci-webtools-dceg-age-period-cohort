$(function(){
	
	random_gen();
	disable_calculate();

        // Post json to server
        $('.post').click(function(){
	  $("#spinner").show(); 
          $("#message").hide(); 
          $.ajax({
            type: 'POST',
            // Provide correct Content-Type, so that Flask will know how to process it.
            contentType: 'application/json',
            // Encode data as JSON.
            //async:false,
            data: JSON.stringify({
              k: $("#independent").val(),
              sens: trim_spaces($("#sensitivity_val").text()),
	      spec: trim_spaces($("#specificity_val").text()),
              prev: $("#prevalence").val(),
              N: $("#n_value").val(),
              unique_id: $("#randomnumber").text(),
	      fixed_flag:$("#fixed_flag").text() 
            }),
            // This is the type of data expected back from the server.
            dataType: 'json',
            url: '/sampleSizeRest/',
            success: function (ret) {
	      $("#spinner").hide();
	      $("#output_graph").empty();
          	generate_tabs($("#fixed").val(),$("#randomnumber").text());
	  	generate_tables(ret);
                random_gen();
      },
	    error: function(jqXHR, textStatus, errorThrown) {
		$("#spinner").hide();
		console.log("header: " + jqXHR + "\n" + "Status: " + textStatus + "\n\nThe server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.");
		message = 'Service Unavailable: ' + textStatus + "<br>";
		message += "The server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.<br>";
		$("#message-content").empty().append(message);	   
		$("#message").show();    
	    },
          });
	//alert("After ajax call");
	return false;
        });
});

$(function(){
	$('.reset').click(function(){
		$('#ss')[0].reset();
	});

	$("#add-test-data").click(function() {
		example_code();
	});	

	$("#contour").keyup(function(){
		change_hidden('contour');
	});

	$("#fixed").keyup(function(){
		change_hidden('fixed');
	});

});


function generate_tables(jsonrtn){
for(var i in jsonrtn) {
    console.log(i);
    var tablesvar = "<TABLE class='table_data'><TBODY>";
    tablesvar += "<TR><TH class='table_data header'>Sensitivity</TH><TH class='table_data header'>Optimal K</TH><TH class='table_data header'>Relative efficiency gain or <br>loss compared to k = 0.5</TH></TR>";
    ppvtabledata = tablesvar;
    cnpvtabledata = tablesvar;
    for (n=0; n<jsonrtn[i]["PPVData"].length; n++) {
    	console.log("PPVData");
    	ppvtabledata += "<TR><TD>"+jsonrtn[i]["PPVData"][n]["Sensitivity"]+"</TD>";
    	ppvtabledata += "<TD>"+jsonrtn[i]["PPVData"][n]["Optimal k"]+"</TD>";
    	ppvtabledata += "<TD>"+jsonrtn[i]["PPVData"][n]['Relative efficiency gain or loss compared to k = 0.5']+"</TD>";
    	console.log("cNPVData");
    	cnpvtabledata += "<TD>"+jsonrtn[i]["cNPVData"][n]["Sensitivity"]+"</TD>";
    	cnpvtabledata += "<TD>"+jsonrtn[i]["cNPVData"][n]["Optimal k"]+"</TD>";
    	cnpvtabledata += "<TD>"+jsonrtn[i]["cNPVData"][n]['Relative efficiency gain or loss compared to k = 0.5']+"</TD></TR>";
    }
    ppvtabledata += "</TBODY></TABLE>";
    cnpvtabledata += "</TBODY></TABLE>";
    $("#"+i+"ppvdata").append(ppvtabledata);
    $("#"+i+"cnpvdata").append(cnpvtabledata);
}
}

function disable_calculate(){
	$('.post').prop("disabled", true);
}

function enable_calculate(){
	$('.post').removeAttr("disabled");
}

function generate_tabs(iterate,randomnumber){
      var fixed_flag = $("#fixed_flag").text();
      var fixedvals=iterate.split(',');
      var arrayLength = fixedvals.length;
      $("#output_graph").empty();
      var tabheaders = "<ul>";
      var tabcontent="";
      var pimagename="PPVkSensSpec-";
      var cimagename="cNPVkSensSpec-";

      var fixedtype=$("#fixed_flag").text();
      console.log("Fixed flag is "+fixedtype);
      if (fixedtype === "Sensitivity"){
          pimagename="PPVkSpecSens-";
          cimagename="cNPVkSpecSens-";
      }
	
      for (var i = 0; i < arrayLength; i++) {
           console.log(fixedvals[i]);
	   tabheaders += '<li><a href="#tab'+(i+1)+'">'+fixed_flag+'<br />'+fixedvals[i]+'</a></li>';
           //tabcontent += '<div id="tabs-'+(i+1)+'"><p>IMAGE'+fixedvals[i]+'</p></div>';
           tabcontent += '<div id="tab'+(i+1)+'"> <TABLE><TR><TD> <TABLE><TR><TD><IMG SRC="/sampleSize/tmp/'+pimagename+randomnumber+'-'+(i+1)+'.png"></TD></TR> <TR><TD><div id="tab'+(i+1)+'ppvdata"><div></TD></TR></TABLE> </TD><TD> <TABLE><TR><TD><IMG SRC="/sampleSize/tmp/'+cimagename+randomnumber+'-'+(i+1)+'.png"></TD></TR> <TR><TD><div id="tab'+(i+1)+'cnpvdata"></div></TD></TR></TABLE> </TD></TR></TABLE> </div>';	  
 
    //Do something
      }    
      tabheaders += "</ul>";
      // First make the right tabs

      tabs = $("<div id='tabs'> </div>");
      $("#output_graph").append(tabs);
      $("#tabs").append(tabheaders);
      $("#tabs").append(tabcontent);
//Now execute
      $("#tabs").tabs();

};

function change_ff(){
	$("#fixed_flag").text($("#fixed_dropdown option:selected").text());
}

function lock_fixed_options(){
	var contour = $("#contour_dropdown option:selected").text();
        $("#fixed_dropdown").empty();
	if (contour === "Specificity"){
                $("#fixed_dropdown").append('<option value="specificity" disabled="disabled">Specificity</a>');
		$("#fixed_dropdown").append('<option value="sensitivity" selected>Sensitivity</a>');
		$("#specificity_val").text($("#contour").val());
                $("#sensitivity_val").text($("#fixed").val());
	}
        if (contour === "Sensitivity"){
                $("#fixed_dropdown").append('<option value="specificity" selected>Specificity</a>');
                $("#fixed_dropdown").append('<option value="sensitivity" disabled="disabled">Sensitivity</a>');
		$("#sensitivity_val").text($("#contour").val());
		$("#specificity_val").text($("#fixed").val());
        }
	change_ff();
}




function change_hidden(callingbox){
		if (((callingbox == "contour")) && ($("#contour_dropdown option:selected").text() == "Specificity")) {   
             	  $("#specificity_val").text(trim_spaces($("#contour").val()));
		}else if (((callingbox == "contour")) && ($("#contour_dropdown option:selected").text() == "Sensitivity")){
		  $("#sensitivity_val").text(trim_spaces($("#contour").val()));
                }else if (((callingbox == "fixed")) && ($("#fixed_dropdown option:selected").text() == "Sensitivity")){
                  $("#sensitivity_val").text(trim_spaces($("#fixed").val()));
                }else if (((callingbox == "fixed")) && ($("#fixed_dropdown option:selected").text() == "Specificity")){
                  $("#specificity_val").text(trim_spaces($("#fixed").val()));
	        }else{
		  return 0;
		}
}

function trim_spaces(varstring){
		return varstring.replace(/\s/g, '');	
}

function example_code(){
                $("#message").hide();
		$("#independent").val("0,1");
		$("#contour").val("0.8,0.9,0.95,0.995");
		$("#contour_dropdown").val("sensitivity");
		$("#fixed").val("0.7,0.8,0.9");
		$("#fixed_dropdown").val("specificity");
		$("#prevalence").val("0.001");
		$("#n_value").val("1");
		$("#fixed_flag").text("Specificity");
		change_hidden("contour");
		change_hidden("fixed");
		enable_calculate();
}

function reset_code(){
                $("#independent").val("0,1");
                $("#contour").val("");
                $("#contour_dropdown").val("");
                $("#fixed").val("");
                $("#fixed_dropdown").val("");
                $("#prevalence").val("");
                $("#n_value").val("");
                $("#fixed_flag").text("");
		$("#output_graph").empty();
		$("#message").empty();
          	$("#message-content").empty();
          	$("#message").hide();
		disable_calculate();
        //        change_hidden("contour");
        //        change_hidden("fixed");
}



function random_gen(){
          var randomno = Math.floor((Math.random() * 1000) + 1);
          $("#randomnumber").text(randomno);
}
