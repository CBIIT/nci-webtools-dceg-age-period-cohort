$(document).ready(function () {
    // Save DOM references to minimize queries
    var cfg = crosstalk.config({
        description: $('#description'),
        startYear: $('#startYear'),
        startAge: $('#startAge'),
        interval: $('#interval'),
        titleA: $('#title1'),
        titleB: $('#title2'),
        inputFileA: $('#inputfile1'),
        inputFileB: $('#inputfile2')
    });

    // Add event listeners
    for (element in cfg)
        cfg[element].change(crosstalk.update);

    window.reset = function (e) {
        e.wrap('<form>').closest('form').get(0).reset();
        e.unwrap();
    }

    $.adamant.pastable.functions["crosstalk-input"] = function (target, data) {
        var data = data.split(/[\r\n]+/);
        while (data[data.length - 1].trim() == "") {
            data.pop();
        }
        data = data.map(function (line) {
            var line = line.split(/[,\t]+/);
            line.map(function (entry) {
                return parseFloat(entry);
            });
            return line;
        });
        var datatarget = $(target).attr('data-target');
        window.reset($('#'+datatarget));
        crosstalk.model[datatarget] = $.extend(crosstalk.model[datatarget] || {}, {
            'table': data,
            'title': $('#' + $('#' + datatarget).attr('data-target')).val()
        });
        $(target).children("textarea").val("");
        crosstalk.update();
    };

    $(document).on("show.bs.tab", function (e) {
        if ($(e.target.parentElement).hasClass("disabled"))
            e.preventDefault();
    });

    $(document).on("shown.bs.tab", function (e) {
        // fix dataTable column widths on tab change
        var tables = $.fn.dataTable.fnTables(true);
        if (tables.length > 0) {
            $.each(tables, function () {
                $(this).dataTable().fnAdjustColumnSizing();
            });
        }
    });

    $(document).on("hidden.bs.modal", function (e) {
        $(e.target).find(".modal-body").empty();
    });

    $("#dataFlip").click(function (e) {
        e.preventDefault();
        var swap = self.model.inputfile2;
        self.model.inputfile2 = self.model.inputfile1;
        self.model.inputfile1 = swap;
        swap = cfg.titleB.val();
        cfg.titleB.val(cfg.titleA.val());
        cfg.titleA.val(swap);
        crosstalk.update();
    });

    $("#helpBtn").on("click", function (e) {
        e.preventDefault();
        window.open("help.html", "Crosstalk Help", "width=750, height=550");
    });

    $("#showSwitch").on("change", function () {
        if (this.checked) {
            $("#irrGraphs img:first").addClass("show");
            $("#irrGraphs img:nth(1)").removeClass("show");
        } else {
            $("#irrGraphs img:first").removeClass("show");
            $("#irrGraphs img:nth(1)").addClass("show");
        }
    });

    $("#showNumber *").on("click keypress", function (e) {
        if (e.keyCode == 13 || e.type == "click") $("#showSwitch").trigger("click");
    });


    $('#apcRatePane > nav li, #gafPane > nav li').on('click keypress', function (e) {
        var target = $(e.delegateTarget);
        var id = target.attr("id");
        target.add($('#' + id.substr(0, id.length - 2))).parent().children().toggleClass('active');
    });

    $('#modelBt').click(crosstalk.validate);

    $('[type="reset"]').click(crosstalk.reset);
    crosstalk.reset();
});

/* ---------- ReadFile Module - Parses an input file as a crosstalk model ---------- */
var readfile = (function () {

    return {
        createModel: createModel
    };

    /* ---------- Reads a file from an input element and returns the generated model as a parameter to the callback ---------- */
    function createModel(element, callback) {
        readFile(element, function (contents) {
            callback(element.id, {
                title: parseHeader(contents.shift()),
                description: parseHeader(contents.shift()),
                startYear: parseInt(parseHeader(contents.shift())),
                startAge: parseInt(parseHeader(contents.shift())),
                interval: parseInt(parseHeader(contents.shift())),
                table: parseTable(contents)
            });
        });
    }

    /* ---------- Reads a file from an input element and returns an array of lines to the callback ---------- */
    function readFile(element, callback) {
        if (window.FileReader) {
            var file = element.files[0];
            var reader = new FileReader();

            if (file)
                reader.readAsText(file);

            reader.onload = function (event) {
                callback(event.currentTarget.result.split(/[\r\n]+/g));
            };
        }
    }

    /* ---------- Parse an array of lines (from a csv) as a matrix of floats ---------- */
    function parseTable(table) {
        while (table[table.length - 1].trim() == "") {
            table.pop();
        }
        return table.map(function (line) {
            return line.split(',').map(function (element) {
                return parseFloat(element);
            });
        });
    }

    /* ---------- Parse a header line - return the portion after the first colon ---------- */
    function parseHeader(line) {
        //        try {
        var description = line.match(/"(.*?)"/);

        if (description)
            line = description[1];
        else
            line = line.split(',')[0];

        if (line.split(/:(.+)?/, 2)[1])
            return line.split(/:(.+)?/, 2)[1].trim();
    }

})();


/* ---------- CrossTalk Module - Syncs the model and calculation results with the UI ---------- */
var crosstalk = (function ($, ReadFile) {
    // set default datatable options
    $.extend(true, $.fn.dataTable.defaults, {
        "destroy": true,
        "bSort": false,
        "bFilter": false,
        "paging": false,
        "responsive": true,
        "dom": 't'
    });
    var self = {};

    // Holds DOM references
    self.cfg = {
        description: null,
        startYear: null,
        startAge: null,
        interval: null,
        titleA: null,
        titleB: null,
        inputFileA: null,
        inputFileB: null
    };

    /* ---------- Updates the UI and Model ---------- */
    self.update = function() {
        var self = this;

        // Updates the model based on the contents of the file
        if (self.type == 'file') {
            ReadFile.createModel(self, updateModel);
        } else {
            syncModel();
        }
    };

    /* ---------- Saves DOM references in the configuration ---------- */
    self.config = function(cfg) {
        self.cfg = cfg;
        return self.cfg;
    };

    self.validate = function() {
        var input1 = self.model.inputfile1;
        var input2 = self.model.inputfile2;

        var valid = crosstalk_form.checkValidity();

        if (input1 && input2) {
            if (input1.table.length != input2.table.length || input1.table[0].length != input2.table[0].length) {
                $('#inputfile1')[0].setCustomValidity("The contents of Data Files must have the same dimensions");
            } else {
                $('#inputfile1')[0].setCustomValidity("");
                $('#inputfile2')[0].setCustomValidity("");
            }
        } else {
            if (!input1 || !input1.table)
                $('#inputfile1')[0].setCustomValidity("Data File 1 is required");
            else
                $('#inputfile1')[0].setCustomValidity("");

            if (!input2 || !input2.table)
                $('#inputfile2')[0].setCustomValidity("Data File 2 is required");
            else
                $('#inputfile2')[0].setCustomValidity("");
        }

        $("#input.tab-pane").children("#errors").remove();
        $(".errors").removeClass("errors");

        var invalids = $("input:invalid, select:invalid");
        if (invalids.length > 0) {

            $.each($("input:invalid, select:invalid"), function (i, el) {
                displayErrors(el);
            });

            $("form").addClass("submitted");
        } else {
            crosstalk.getData();
        }
    }
    
    self.failure = function(xhr, error, statusText) {
        var message = "";
        switch (xhr.status) {
        case 503:
            message = "The service that the request is trying to reach is currently down, Please try again later";
            break;
        case 404:
            message = "The request returned with a response of  '" + statusText + "'. Please try again later.";
            break;
        case 400:
            message = xhr.responseJSON.data;
            break;
        default:
            message = "The request has failed for an unknown reason";
            break;
        }

        $(".tab-pane#input").children("#error").remove().prepend($("<div id='error' class='alert alert-danger'>").html(message));
    }
    
    self.getData = function() {
        $.ajax({
            method: "POST"
            , url: crosstalk_form.action
            , data: JSON.stringify(crosstalk.model)
            , beforeSend: function () {
                $(".loading").css("display", "block");
                $(".tab-content").children("#error").remove();
            }
        }).done(function (data) {
            $(".graphContainers, .title").empty();
            var result = data;
            for (var key in result.data) {
                var resultSet = result.data[key];

                if (key == "IncidenceRates")
                    incRatesTab(resultSet);
                else if (key == "IncidenceRateRatios")
                    incRateRatioTab(resultSet);
                else if (key == "ApcOfIncidenceRates")
                    apcRatesTab(resultSet);
                else if (key == "ApcOfRateRatios")
                    apcRateRatioTab(resultSet);
                else if (key == "GoodnessOfFit")
                    goodFitTab(resultSet);

                $('#ratePane,#showPlot, #apcRatePane, #apcRatioPane').addClass('show');
            }
            $(".output").addClass("show");
            $("ul.nav.nav-pills li").removeClass("disabled");

            // bind events to newly generated images
            $(".expandImg").on("click keypress", previewImage);
        }).fail(crosstalk.failure).always(function () {
            $(".loading").css("display", "none");
        });
    }

    self.reset = function(e) {
        // Holds values from the DOM in a model
        self.model = {
            description: null,
            startYear: null,
            startAge: null,
            interval: null,
            titleA: null,
            titleB: null,
            inputfile1: null,
            inputfile2: null
        };
        if (e !== undefined) e.preventDefault();

        $(".tab-pane#input").children("#errors").remove();
        $(".submitted").removeClass("submitted");

        $(".graphContainers").empty();
        $('.output').removeClass('show');

        $("ul.nav.nav-pills li").not(":first").not(":last").addClass("disabled");

        $('#description,#startAge,#startYear,#interval,#title1,#title2').val("");
        crosstalk.update();

        $(".tablesContainers table").each(function () {
            if ($.fn.DataTable.isDataTable(this)) {
                $(this).DataTable().destroy();
            }
            $(this).empty();
            $(".title").empty();
        });
        var matrix = [];
        for (var i = 0; i < 13; i++) {
            var arr = [];
            for (var j = 0; j < 7; j++) {
                arr.push("&zwj;");
            }
            matrix.push(arr);
        }
        createInputTable("#dataset1", createHeaders(3), matrix);
        createInputTable("#dataset2", createHeaders(3), matrix);
        $("#dataset1 .paste-here, #dataset2 .paste-here").remove();
        $("#dataset1 textarea, #dataset2 textarea").before('<img class="img-responsive paste-here" alt="paste here" src="/common/images/paste-here.gif"/>');

        $(".tab-content").children("#error").remove();
    }

    return self;


    function createOutputHeaders(initial, headers) {
        var returnHeaders = $.extend(true, [], initial);
        for (var i = 0; i < headers.length; i++) {
            returnHeaders.push({
                data: headers[i].toString()
                , title: headers[i].toString()
            });
        }
        return returnHeaders;
    }

    function createGraphImage(containerId, link, ratio) {
        var ratio = ratio || 1;
        var width = parseInt(12 * ratio);
        $(containerId).append('<div class="col-sm-' + width + '"><div class="graphContainers"><a class="expandImg" data-toggle="modal" data-target="#imgPreview" tabindex="0"><img class="img-responsive" src="' + link + '" alt="generated graph image"/></a></div></div>');
    }

    function createOutputTable(containerId, title, table, headers, extraHeaders) {
        var extraHeaders = extraHeaders || "";
        var target = $(containerId);
        if ($.fn.DataTable.isDataTable(containerId)) {
            target.DataTable().destroy();
        }
        target.empty().prev('h4').remove();
        if (title) {
            var title = (title.length > 100) ? (title.substr(0, 100) + "...") : title;
            target.before("<h4 class='title'>" + title + "</h4>");
        }
        var dTbl = target.DataTable({
            "data": table
            , "columns": headers
        });

        $(dTbl.table().header()).prepend(extraHeaders);
        target.dataTable().fnDraw();
    }

    function createDatasetLink(containerId, sectionName, displayTitle, table, ratio) {
        var ratio = ratio || 1;
        var data = "";
        for (var i in table) {
            for (var j in table[i]) {
                data += table[i][j] + ",";
            }
            data += "\n";
        }
        var blob = new Blob([data], {
            type: "text/csv;charset=UTF-8"
        });
        var width = parseInt(12 * ratio);
        var link = $(containerId).append('<div class="tabledownload col-sm-' + width + '"><a href="javascript:void(0);" id="findme">View Dataset ' + displayTitle + '</a></div>').find("#findme");
        link = link.removeAttr("id")[0];
        link.href = window.URL.createObjectURL(blob);
        link.download = sectionName + " for " + displayTitle + ".csv";

    }

    function incRatesTab(result) {
        // result has tables, headers and graphs properties
        var headers = createOutputHeaders([{
            data: "_row"
            , title: "Age Group"
        }], result.headers);

        $("#rateTables .title").html("");
        var extraHeaders = "<tr role='row'><th class='header'>Rate</th><th class='header' colspan='" + (headers.length - 1) + "'>Calendar Period</th></tr>";

        if (result.tables[0]) {
            createOutputTable("#rateTable1", self.model.titleA, result.tables[0], headers, extraHeaders);
            createGraphImage("#rateGraphs", result.graphs[0], .5);
        }

        if (result.tables[1]) {
            createOutputTable("#rateTable2", self.model.titleB, result.tables[1], headers, extraHeaders);
            createGraphImage("#rateGraphs", result.graphs[1], .5);
        }
    }

    function incRateRatioTab(result) {
        // result has tables, headers and graphs properties
        var headers = createOutputHeaders([{
            data: "_row"
            , title: "Age Group"
        }], result.headers);

        var extraHeaders = "<tr role='row'><th class='header'>Rate</th><th class='header' colspan='" + (headers.length - 1) + "'>Calendar Period</th></tr>";

        if (result.tables[0]) {
            createOutputTable("#irrTable", (self.model.titleA + " vs " + self.model.titleB), result.tables[0], headers, extraHeaders);

            createGraphImage("#irrGraphs", result.graphs[0][0]);
            createGraphImage("#irrGraphs", result.graphs[1][0]);

            if (!$("#showSwitch").is(":checked")) {
                $("#irrGraphs img:nth-child(2)").addClass("show");
            } else {
                $("#irrGraphs img:first").addClass("show");
            }
        }
    }

    function apcRatesTab(result) {
        $("#collapseOne, #collapseTwo, #collapseThree, #collapseFour").children(".panel-body").add("#local-content").empty();
        if (result.AdjustedRates) {
            var ar = result.AdjustedRates;
            if (ar.ComparisonOfAdjustedRates) {
                var coarTarget = $("#collapseOne .panel-body");
                var coar = ar.ComparisonOfAdjustedRates;
                var headers = createOutputHeaders([{
                    data: "_row"
                    , title: "Null Hypothesis"
                }], coar.headers);
                if (coar.graphs) {
                    createGraphImage(coarTarget, coar.graphs[0][0], .5);

                }
                if (coar.tables) {
                    $(coarTarget).append('<div class="col-sm-6"><table id="coarTable" class="data-table stripe compact" width="100%"></table></div>');
                    createOutputTable("#coarTable", (self.model.titleA + " vs " + self.model.titleB), coar.tables[0], headers);
                }
            }
            if (ar.CrossSectionalAgeCurve) {
                var csacTarget = $("#collapseFour .panel-body");
                var csac = ar.CrossSectionalAgeCurve;
                if (csac.graphs) {
                    createGraphImage(csacTarget, csac.graphs[0][0]);
                }
                if (csac.tables) {
                    for (var i in csac.tables) csac.tables[i].unshift(csac.headers);
                    createDatasetLink(csacTarget, "Cross Sectional Age Curve", self.model.titleA, csac.tables[0], .5)
                    createDatasetLink(csacTarget, "Cross Sectional Age Curve", self.model.titleB, csac.tables[1], .5)
                }
            }
            if (ar.FittedCohortPattern) {
                var fcpTarget = $("#collapseTwo .panel-body");
                var fcp = ar.FittedCohortPattern;
                if (fcp.graphs) {
                    createGraphImage(fcpTarget, fcp.graphs[0][0]);
                }
                if (fcp.tables) {
                    for (var i in fcp.tables) fcp.tables[i].unshift(fcp.headers);
                    createDatasetLink(fcpTarget, "Fitted Cohort Pattern", self.model.titleA, fcp.tables[0], .5)
                    createDatasetLink(fcpTarget, "Fitted Cohort Pattern", self.model.titleB, fcp.tables[1], .5)
                }
            }
            if (ar.FittedTemporalTrends) {
                var fttTarget = $("#collapseThree .panel-body");
                var ftt = ar.FittedTemporalTrends;
                if (ftt.graphs) {
                    createGraphImage(fttTarget, ftt.graphs[0][0]);
                }
                if (ftt.tables) {
                    for (var i in ftt.tables) ftt.tables[i].unshift(ftt.headers);
                    createDatasetLink(fttTarget, "Fitted Temporal Trends", self.model.titleA, ftt.tables[0], .5)
                    createDatasetLink(fttTarget, "Fitted Temporal Trends", self.model.titleB, ftt.tables[1], .5)
                }
            }
        }
        if (result.LocalDrifts) {
            var ld = result.LocalDrifts;
            if (ld.graphs) {
                createGraphImage("#local-content", ld.graphs[0][0], .5);
                createGraphImage("#local-content", ld.graphs[1][0], .5);
            }
            if (ld.tables) {
                for (var i in ld.tables) ld.tables[i].unshift(ld.headers);
                createDatasetLink("#local-content", "Local Drifts", self.model.titleA, ld.tables[0], .5)
                createDatasetLink("#local-content", "Local Drifts", self.model.titleB, ld.tables[1], .5)

            }
        }
        if (result.NetDrifts) {
            var nd = result.NetDrifts;
            createOutputTable("#netTable", undefined, nd.tables[0], createOutputHeaders({}, nd.headers));
        }
    }

    function apcRateRatioTab(result) {
        $("#csac, #fcp, #ftt").children(".panel-body").empty()
        if (result.CrossSectionalAgeCurve) {
            var csac = result.CrossSectionalAgeCurve;
            createGraphImage("#csac .panel-body", csac.graphs[0][0]);
        }
        if (result.FittedCohortPattern) {
            var fcp = result.FittedCohortPattern;
            createGraphImage("#fcp .panel-body", fcp.graphs[0][0]);
        }
        if (result.FittedTemporalTrends) {
            var ftt = result.FittedTemporalTrends;
            createGraphImage("#ftt .panel-body", ftt.graphs[0][0]);
        }
        if (result.IO) {
            var io = result.IO;
            createOutputTable("#interceptTable", (self.model.titleA + " vs " + self.model.titleB), io.tables[0], createOutputHeaders({}, io.headers));
            createGraphImage("#intercept .graphsContainers", io.graphs[0][0]);
        }
    }

    function goodFitTab(result) {
        $("#gaf_pop1li a, #gaf_pop2li a").empty();

        $("#gaf_pop1li a, #gaf_pop1 .title").text(self.model.titleA);
        $("#gaf_pop2li a, #gaf_pop2 .title").text(self.model.titleB);

        var graphs = result.graphs;

        createGraphImage("#gaf_pop1", graphs[0][0], .5);
        createGraphImage("#gaf_pop1", graphs[0][1], .5);
        createGraphImage("#gaf_pop2", graphs[1][0], .5);
        createGraphImage("#gaf_pop2", graphs[1][1], .5);
    }

    function previewImage(e) {
        var img = $(e.target)[0];

        if (e.target.tagName != "IMG")
            img = $(e.target).find("img")[0];

        $("#imgPreview .modal-body").append("<img class='img-responsive' src='" + img.src + "' />");
        if (e.keyCode == 13) $("#imgPreview").modal("show");
    }

    /* ---------- Updates the model with contents from the UI ---------- */
    function syncModel() {
        self.model.description = self.cfg.description.val();
        self.model.startYear = parseFloat(self.cfg.startYear.val());
        self.model.startAge = parseFloat(self.cfg.startAge.val());
        self.model.interval = parseFloat(self.cfg.interval.val());
        self.model.titleA = self.cfg.titleA.val();
        self.model.titleB = self.cfg.titleB.val();
        if (self.model.inputfile1) {
            self.model.inputfile1.title = self.model.titleA;
            table_input($("#dataset1"), self.model.inputfile1);
        }
        if (self.model.inputfile2) {
            self.model.inputfile2.title = self.model.titleB;
            table_input($("#dataset2"), self.model.inputfile2);
        }
    }

    function create_line(line, i) {
        var lead = "&zwj;";
        if (self.model.startAge && self.model.interval) {
            var age = self.model.startAge + (self.model.interval * i);
            lead = age + "-" + (age + self.model.interval - 1);
        }
        line.unshift(lead);
        return line;
    }

    function table_input(target, data) {
        var thisElement = $(target);
        var tableData = $.extend(true, [], data.table);
        if (tableData.length < 1) {
            alert('needs a message 1');
            return;
        }
        var width = tableData[0].length + 1;
        for (var i in tableData) {
            tableData[i] = create_line(tableData[i], i);
            if (tableData[i].length != width) {
                alert('needs a message 2');
                return;
            }
        }
        var table = createInputTable("#" + thisElement.prop("id"), createHeaders((width - 1) / 2, tableData), tableData).children('thead');
        if (self.model.startYear && self.model.interval) {
            var headerRow = $('<tr><th class="white"></th></tr>');
            for (var i = 0; i < (width - 1) / 2; i++) {
                var header = self.model.startYear + self.model.interval * i;
                headerRow.append($('<th class="header" colspan="2"></th>').html(header + "-" + (header + self.model.interval - 1)));
            }
            table.prepend(headerRow);
        }
        if (data.title && self.model.description) {
            var title = (data.title.length > 100) ? (data.title.substr(0, 100) + "...") : data.title;
            var desc = (self.model.description.length > 100) ? (self.model.description.substr(0, 100) + "...") : self.model.description;

            table.prepend('<tr><th class="white"></th><th class="header" colspan="' + (width - 1) + '">' + title + '<br/><span class="blue">' + desc + '</span></th></tr>');
        }
        thisElement.children(".paste-here").remove();
    }

    /* ---------- Saves the file contents to the model ---------- */
    function updateModel(id, contents) {
        var checkAgainst = { "inputfile1": self.model["inputfile2"], "inputfile2": self.model["inputfile1"]}[id];
        var overwrite = true;
        if (checkAgainst) {
            var missing = [];
            if (contents.description != self.cfg.description) missing[missing.length] = "Description";
            if (contents.startYear != self.cfg.startYear) missing[missing.length] = "Start Year";
            if (contents.startAge != self.cfg.startAge) missing[missing.length] = "Start Age";
            if (contents.interval != self.cfg.interval) missing[missing.length] = "Interval (Years)";
            if (missing.length > 0) {
                missing[Math.max(missing.length-2,0)] = missing.splice(-2).join(", and ");
                overwrite = confirm("The "+missing.join(", ")+" header"+(missing.length>1?"s":"")+" in this file do not match the currently entered values. Click \"Okay\" to overwrite them.");
            }
        }
        self.model[id] = contents;
        if (overwrite) {
            self.cfg.description.val(contents.description);
            self.cfg.startAge.val(contents.startAge);
            self.cfg.startYear.val(contents.startYear);
            self.cfg.interval.val(contents.interval);
        }
        if (self.model.inputfile1)
            self.cfg.titleA.val(self.model.inputfile1.title);
        if (self.model.inputfile2)
            self.cfg.titleB.val(self.model.inputfile2.title);
        syncModel();
    }

    function createHeaders(length, data) {
        var ageHeader = "Age";
        if (data !== undefined) ageHeader = '<span class = "ageGroups">' + data.length + " age groups </span>";
        var headers = [{
            title: ageHeader
            , className: 'dt-center grey'
        }];
        for (var i = 0; i < length; i += 1) {
            headers.push({
                title: "Count"
                , className: 'dt-body-right'
            });
            headers.push({
                title: "Population"
                , className: 'dt-body-right'
            });
        }
        return headers;
    };

    function createInputTable(containerID, headers, data) {
        var table = $(document.createElement('table'));
        table.attr('class', 'table display compact');
        table.attr('width', '100%');
        $(containerID).children(".dataTables_wrapper").children(".dataTable").DataTable().destroy();
        $(containerID).children("table").remove();
        $(containerID).prepend(table);
        table.DataTable({
            "data": data
            , "columns": headers
            , "scrollX": false
        });
        $(containerID).find('#inputTable_wrapper').addClass('table-responsive');
        $(containerID).find('.table').addClass('input-table');
        return table;
    };
})($, readfile);


function displayErrors(el) {
    if ($("#input.tab-pane #errors").length === 0)
        $("#input.tab-pane").prepend("<div id='errors' class='alert alert-danger'></div>");

    var label = $("label[for='" + el.id + "']");
    label.addClass("errors");
    var msg = "";

    if (el.validity.badInput) {
        msg += "'" + label[0].innerText + "' contains an invalid value </br>";
    }
    if (el.validity.valueMissing) {
        msg += "'" + label[0].innerText + "' is required </br>";
    }

    if (el.validity.customError) {
        msg += el.validationMessage + "</br>";
    }

    $("#errors").append(msg);
}
