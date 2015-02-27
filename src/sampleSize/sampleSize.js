$(function(){
	
	random_gen();
	disable_calculate();
	$("#spinner").hide();
        $("#message").hide();

        // Post json to server
        $('.post').click(function(){
	  //var start = new Date();
          $("#output_graph").empty();
//	  spinner = $('<div><p id="spinner"><i class="fa fa-spinner fa-spin fa-2x"></i><span id="spinnerText">Calculating</span></p></div>');
//	  $("#output_graph").append(spinner); 

	  $("#message-content").empty();
          $("#message").hide();
	  $("#spinner").show();
          $.ajax({
            type: 'POST',
            // Provide correct Content-Type, so that Flask will know how to process it.
            contentType: 'application/json',
            // Encode data as JSON.
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
              random_gen();
 	      //var end = new Date();
              //var secondsOpen = Math.floor((end - start) / 1000);
              //$("#benchmark").text(secondsOpen);
            },
	    error: function(jqXHR, textStatus, errorThrown) {
		console.log("header: " + jqXHR + "\n" + "Status: " + textStatus + "\n\nThe server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.");
		message = 'Service Unavailable: ' + textStatus + "<br>";
		message += "The server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.<br>";
		$("#message-content").empty().append(message);	   
		$("#message").show();    
	        $("#spinner").hide();
	    },
          });
        });

	$('.reset').click(function(){
		reset_code();
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
	   tabheaders += '<li><a href="#tabs-'+(i+1)+'">'+fixed_flag+'<br />'+fixedvals[i]+'</a></li>';
           //tabcontent += '<div id="tabs-'+(i+1)+'"><p>IMAGE'+fixedvals[i]+'</p></div>';
           tabcontent += '<div id="tabs-'+(i+1)+'"><p><IMG SRC="/sampleSize/tmp/'+pimagename+randomnumber+'-'+(i+1)+'.png">&nbsp;&nbsp;<IMG SRC="/sampleSize/tmp/'+cimagename+randomnumber+'-'+(i+1)+'.png"></p></div>';	   
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
                $("#message-content").empty();
                $("#message").hide();
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
