$( document ).ready(function() {
  var createHeaders = function(length,data) {
    var ageHeader = "Age";
    if (data !== undefined) ageHeader = '<span class = "ageGroups">' + data.length + " age groups </span>";
    var headers = [{
      title: ageHeader,
      className: 'dt-center grey'
    }];
    for (var i = 0; i < length; i += 1) {
      headers.push({
        title: "Count",
        className: 'dt-body-right'
      });
      headers.push({
        title: "Population",
        className: 'dt-body-right'
      });
    }
    return headers;
  };
  
  var createInputTable = function(containerID, headers, data) {
    var table = document.createElement('table');

    table.setAttribute('class', 'table display compact');
    table.setAttribute('width', '100%');
    $(containerID).children(".dataTables_wrapper").children(".dataTable").DataTable().destroy();
    $(containerID).children("table").remove();
    $(containerID).prepend(table);
    $(table).DataTable({
        "destroy": true,
        "data": data,
        "columns": headers,
        "bSort": false,
        "bFilter": false,
        "paging": false,
        "responsive": true,
        "dom": 't'
    });

    $(containerID).find('#inputTable_wrapper').addClass('table-responsive');
    $(containerID).find('.table').addClass('input-table');
  };
  var matrix = [];
  for (var i = 0; i < 13; i++) {
    var arr = [];
    for (var j = 0; j < 7; j++) {
      arr.push("&zwj;");
    }
    matrix.push(arr);
  }
  createInputTable("#dataset1",createHeaders(3),matrix);
  createInputTable("#dataset2",createHeaders(3),matrix);
  var table_input = function(target,data) {
    var target = $(target);
    target.children("textarea").val("");
    var data = data.split(/[\r\n]+/);
    if (data.length < 1) {
      alert('needs a message 1');
      return;
    }
    if (data[data.length-1] == "")
       data.pop();
    data[0] = data[0].split(/[ \t,]+/);
    var width = data[0].length;
    data[0].unshift("&zwj;");
    for (var i = 1; i < data.length; i++) {
      data[i] = data[i].split(/[ \t,]+/)
      if (data[i].length != width) {
        alert('needs a message 2');
        return;        
      }
      data[i].unshift("&zwj;");
    }
    createInputTable("#"+target.prop("id"),createHeaders(width/2,data),data);
    target.children(".paste-here").remove();
  };
  var parseHeader = function(line) {
    var description = line.match(/"(.*?)"/);
    if (description)
      line = description[1];
    else
      line = line.split(',')[0];
    return line.split(/: (.+)?/, 2)[1].trim();
  };
  var file_input = function(target,data) {
    var target = $(target);
    var data = data.substr(13);
    try {
      data = atob(data).split(/[\r\n]+/);
    } catch (e) {
      alert('needs a message 4');
      return;
    }
    var title = parseHeader(data.shift());
    var description = parseHeader(data.shift());
    var startYear = parseHeader(data.shift());
    var startAge = parseHeader(data.shift());
    var interval = parseHeader(data.shift());
    data = data.join("\n");
    console.log(target);
    table_input(target,data);
  }
  var file_read = function(target,files) {
    for (var i = 0; i < files.length; i++) {
      var file = files[0];
      if (file.name.substr(-4) != ".csv") {
        alert('needs a message 3');
        return;
      }
      var reader = new FileReader();
      reader.onload = function(e) {file_input(target,e.target.result);};
      reader.readAsDataURL(file);
    }
  }
  $.adamant.pastable.functions["crosstalk-input"] = table_input;
  $('#dataset1, #dataset2').on('drop',function(e) {
    e.preventDefault();
    var target = $(e.delegateTarget);
    var files = e.originalEvent.dataTransfer.files;
    file_read(target,files);
  });
  $('#inputfile1, #inputfile2').on('change',function(e) {
    var target = $(e.target);
    file_read('#'+target.attr("data-target"),target[0].files);
    target.wrap("<form>").closest("form")[0].reset();
    target.unwrap();
  });
	//var spinbox1 = new SpinBox('sbox1', {'minimum' : 1, 'maximum' : 5});
	//var spinbox2 = new SpinBox('sbox2', {'minimum' : 1, 'maximum' : 5});
	//var spinbox3 = new SpinBox('sbox3', {'minimum' : 1, 'maximum' : 5});
	//$("[name='showSwitch']").bootstrapSwitch();

	$("#modelBt").click(function() {
		$('#ratePane').attr('style', 'display:block');
		$('#showNumber').attr('style', 'display:block');
		$('#showPlot').attr('style', 'display:block');
		$('#apcRatePane').attr('style', 'display:block');
		$('#apcRatioPane').attr('style', 'display:block');

	});

	$("#resetBt").click(function() {
		$('#ratePane').attr('style', 'display:none');
		$('#showNumber').attr('style', 'display:none');
		$('#showPlot').attr('style', 'display:none');
		$('#apcRatePane').attr('style', 'display:none');
		$('#apcRatioPane').attr('style', 'display:none');

	});

    $('#crosstalk-tab-nav').tabCollapse();

	$('#startYear').spinner({
        min: 1800,
        max: 2200,
        step: 1,
        spin: function(event, ui) {
			// Needs redraw table code
        },
        stop: function(event, ui) {
			// Needs redraw table code
        }
    });

	$('#startAge').spinner({
        min: 0,
        max: 120,
        step: 1,
        spin: function(event, ui) {
			// Needs redraw table code
        },
        stop: function(event, ui) {
			// Needs redraw table code
        }
    });
});

  function changeContent(id){
			hideAll();
			$("#"+id).show();
			$("#"+id +"li").attr("class", "active");
	}
	function hideAll(){
		$("#local").hide();
		$("#net").hide();
		$("#adjust").hide();
		$("#localli").attr("class", "");
		$("#netli").attr("class", "");
		$("#adjustli").attr("class", "");
	}

	function Slide_menu_Horz(action)
	 {
	        if($("#icon").hasClass("fa fa-caret-left fa-2x")||action=='hide')
	        {
	         $('#icon').removeClass("fa fa-caret-left fa-2x");
	         $('#icon').addClass("fa fa-caret-right fa-2x");
	         $("#slideoutForm").fadeOut(300);


	         $("#icon").animate({
	                marginLeft: '1%'
	                }, 300);

	        $("#slideout").animate({
	                transform: 'translate(-400px, 0px)'
	                }, 300);

	        setTimeout(function(){
	                $("#right_panel").animate({
	                width: '100%'
	                        }, 300);
	                }, 600);
	    }
	    else if($("#icon").hasClass("fa fa-caret-right fa-2x")||action=='show')
	        {
	         $('#icon').removeClass("fa fa-caret-right fa-2x");
	         $('#icon').addClass("fa fa-caret-left fa-2x");
	         $("#slideoutForm").fadeIn(500);
	         $("#icon").animate({
	                marginLeft: '31%'
	                }, 20);

	         $("#right_panel").animate({ width: '66.666666%'}, 10);
	    }
	 }
