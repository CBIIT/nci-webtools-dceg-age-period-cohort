var Glossary = {
	AUC : {
		fullName : "Area under the receiver operator characteristic curve",
		definition : " for a biomarker is the average sensitivity (or, equivalently, the integral of the sensitivity) in the interval of cSpecificity from 0 to 1 (specificity from 1 to 0), itself equal to the area between the ROC curve and the x-axis."
	},
	CV : {
		fullName : "Coefficient of Variation",
		definition : "The coefficient of variation is defined as the ratio of the standard deviation to the mean. It shows the extent of variability in relation to mean of the population."
	},
	cNPV : {
		fullName : "Complement of Negative Predictive Value (cNPV)",
		definition : "Probability of disease, given a negative test result from biomarker. Unlike sensitivity and specificity, cNPV's reflect disease prevalence and is useful for risk stratification."
	},
	Delta : {
		fullName : "Delta",
		definition : "The statistic delta is the ratio of the absolute difference in average level of the biomarker between cases and controls in units of standard deviation."
	},
	DP : {
		fullName : "Disease Prevalence",
		definition : "Proportion of the population with disease, or previously diagnosed with disease, at a given time."
	},
	LR : {
		fullName : "Likelihood Ratios (LR)",
		definition : " The likelihood ratios are factors that update the prior odds to obtain conditional odds of disease after a positive and negative disease."
	},
	LRP : {
		fullName : "Likelihood Ratio Positive (LR+)",
		definition : "The LR+ is the ratio of the probabilities of a case having a positive test (Sensitivity) and of a control having a positive test (cSpecificity)."
	},
	LRN : {
		fullName : "Likelihood Ratio Negative (LR-)",
		definition : "The LR- is the ratio of the probabilities of the control having a negative test (Specificity) and the case having a negative test (cSensitivity)."
	},
	PPV : {
		fullName : "Positive Predictive Value (PPV)",
		definition : "Probability of disease, given a positive test result from biomarker.  Unlike sensitivity and specificity, PPV’s reflect disease prevalence and is useful for risk stratification."
	},
	PPVmcNPV : {
		fullName : "PPV-cNPV",
		definition : "The difference PPV-cNPV is a simple measure of the clinical value of the test, or the difference between risks; if PPV is close to cNPV, the screening test will not be very helpful, even if the sensitivity and specificity are high."
	},
	ROC : {
		fullName : "Receiver operator characteristic (ROC) curve",
		definition : "A presentation that plots a point for all possible thresholds of the biomarker, with the y-axis representing sensitivity and the x-axis representing 1 - <i>specificity</i> of the test.  The ROC curve graphically displays the tradeoff of increased sensitivity but decreased specificity from lowering the threshold, and vice versa."
	},
	Risk : {
		fullName : "Risk",
		definition : "Probability of disease, implicitly prevalent disease, or incident disease within an interval."
	},
	Spec : {
		fullName : "Specificity",
		definition : "Specificity is the proportion whose biomarker test is negative (below the threshold) among those without disease."
	},
	Sens : {
		fullName : "Sensitivity",
		definition : "Sensitivity is the proportion whose biomarker test is positive (above the threshold) among those who are positive for disease."
	}
}

$(document).ready(
		function() {
			// this only takes care of the input page, needs a separate bind for
			// the output page
			bindTermToDefine();
			var htmlText = "";
			for ( var abbrev in Glossary) {
				var term = Glossary[abbrev];
				htmlText += "<b>" + term.fullName + ":</b> " + term.definition
						+ "<br/><br/>";
			}
			$('#glossary').html(htmlText);
		});

function openHelpWindow(pageURL) {
	var helpWin = window
			.open(pageURL, "Help",
					"alwaysRaised,dependent,status,scrollbars,resizable,width=1000,height=800");
	helpWin.focus();
}

/* bind click and mouseout events to class termToDefine and associated id */
function bindTermToDefine() {
	$('div.termToDefine').on(
			"click",
			function() {
				var id = $(this).attr("id");
				//html5 data-attributes
				var termName=$(this).data("term");
				// if termName matches abbreviation in glossary
				var term = Glossary[termName];
				var termToDefineElem = $("#" + id);
				var termDefinitionElem = $("#" + id + "Definition");
				termDefinitionElem.html("<h3>" + term.fullName + "</h3>"
						+ term.definition);
				termDefinitionElem.show();
				//place definition popup at a position left and top down 20 aligned to the center of the id 
				termDefinitionElem.position({
					my : "left top+20",
					at : "center",
					of : $("#"+id),
				});
				$("#"+id).addClass("enlarge");
				
			}).on("mouseout", function() {
		var id = $(this).attr("id");
		var termToDefineElem = $("#" + id);
		var termDefinitionElem = $("#" + id + "Definition");

		termDefinitionElem.hide();
		$("#"+id).removeClass("enlarge");
	});
}

