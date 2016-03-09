$( document ).ready(function() {
	var spinbox1 = new SpinBox('sbox1', {'minimum' : 1, 'maximum' : 5});
	var spinbox2 = new SpinBox('sbox2', {'minimum' : 1, 'maximum' : 5});
	var spinbox3 = new SpinBox('sbox3', {'minimum' : 1, 'maximum' : 5});
	$("[name='showSwitch']").bootstrapSwitch();

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

	         $("#right_panel").animate({
	                width: '66.666666%'
	                        }, 10);



	    }

	 }
