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
        inputFileB: $('#inputfile2'),
    });

    // Add event listeners
    for (element in cfg)
        cfg[element].change(crosstalk.update);

    $.adamant.pastable.functions["crosstalk-input"] = function (target, data) {
        var data = data.split(/[\r\n]+/);
        while (data[data.length - 1].trim() == "") {
            data.pop();
        }
        data = data.map(function (line) {
            var line = line.split(",");
            line.map(function (entry) {
                return parseFloat(entry);
            });
            return line;
        });
        var datatarget = $(target).attr('data-target');
        self.model[datatarget] = $.extend(self.model[datatarget] || {}, {
            'table': data,
            'title': $('#' + $('#' + datatarget).attr('data-target')).val()
        });
        $(target).children("textarea").val("");
        crosstalk.update();
    };

    $(document).on("shown.bs.tab", function (event, ui) {
        // fix dataTable column widths on tab change
        var tables = $.fn.dataTable.fnTables(true);
        if (tables.length > 0) {
            $.each(tables, function () {
                $(this).dataTable().fnAdjustColumnSizing();
            });
        }
    });

    $('#dataset1, #dataset2').on('drop', function (e) {
        e.preventDefault();
        crosstalk.drop(e);
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

    $('#startYear').spinner({
        min: 1800,
        max: 2200,
        step: 1,
        spin: crosstalk.update,
        stop: crosstalk.update
    });

    $('#startAge').spinner({
        min: 0,
        max: 120,
        step: 1,
        spin: crosstalk.update,
        stop: crosstalk.update
    });

    $('#interval').spinner({
        min: 1,
        max: 10,
        step: 1,
        spin: crosstalk.update,
        stop: crosstalk.update
    });

    $("#showSwitch").on("change", function () {

        if (this.checked) {
            $(this.labels[0]).addClass("btn-primary");
            $(this.labels[0]).removeClass("btn-default");

            $("#on, #irrGraphs img:first").addClass("show");
            $("#off, #irrGraphs img:nth(1)").removeClass("show");
        } else {
            $(this.labels[0]).addClass("btn-default");
            $(this.labels[0]).removeClass("btn-primary");

            $("#on, #irrGraphs img:first").removeClass("show");
            $("#off, #irrGraphs img:nth(1)").addClass("show");
        }

    });

    $('#process').click(crosstalk.log);

    $('#modelBt').click(crosstalk.getData);

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
        var description = line.match(/"(.*?)"/);

        if (description)
            line = description[1];
        else
            line = line.split(',')[0];

        return line.split(/:(.+)?/, 2)[1].trim();
    }

})();


/* ---------- CrossTalk Module - Syncs the model and calculation results with the UI ---------- */
var crosstalk = (function ($, ReadFile) {
    var self = this;

    // Holds DOM references
    self.cfg = {
        description: null,
        startYear: null,
        startAge: null,
        interval: null,
        titleA: null,
        titleB: null,
        inputFileA: null,
        inputFileB: null,
    };

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


    // set default datatable options
    $.extend(true, $.fn.dataTable.defaults, {
        "destroy": true,
        "bSort": false,
        "bFilter": false,
        "paging": false,
        "responsive": true,
        "dom": 't',
        "scrollX": true
    });

    return {
        drop: drop,
        update: update,
        config: config,
        log: log,
        getData: getData,
        reset: reset,
        model: self.model,
        dtDefaults: self.tableDefaults
    };

    function getData() {
        if (self.model.inputfile1 && self.model.inputfile2 && self.model.inputfile1.table.length == self.model.inputfile2.table.length && self.model.inputfile1.table[0].length == self.model.inputfile2.table[0].length) {
            $.ajax({
                method: "POST",
                url: crosstalk_form.action,
                data: JSON.stringify(crosstalk.model),
                beforeSend: function () {
                    $(".loading").css("display", "block");
                }
            }).done(function (data) {
                $("#rateGraphs, #irrGraphs, #apcRateGraphs").empty();
                var result = data;
                for (var key in result.data) {
                    var resultSet = result.data[key];

                    if (key == "IncidenceRates")
                        incRatesTab(resultSet);
                    else if (key == "IncidenceRateRatios")
                        incRateRatioTab(resultSet);
                    else if (key == "ApcOfIncidenceRates")
                        apcRatesTab(resultSet);
                    $('#ratePane,#showPlot, #apcRatePane, #apcRatioPane').attr('style', 'display:block');
                }
            }).fail(function () {
                console.log("failed");
            }).always(function () {
                $(".loading").css("display", "none");
            });
        } else {
            alert("needs a message 3")
        }
    }

    function createOutputHeaders(initial, headers) {
      var returnHeaders = $.extend(true,[],initial);
      for (var i = 0; i < headers.length; i++) {
        returnHeaders.push({
          data: headers[i].toString(),
          title: headers[i].toString()
        });
      }
      return returnHeaders;
    }

    function createGraphImage(containerId, link, ratio=.5) {
        var width = parseInt(12*ratio);
        $(containerId).append("<img class='img-responsive col-sm-" + width + "' src='" + link + "' />")
    }
    
    function createOutputTable(containerId,title,table,headers,extraHeaders="") {
        var target = $(containerId);
        if ($.fn.DataTable.isDataTable(containerId)) {
            target.DataTable().destroy();
        }
        target.empty().prev('h4').remove();
        target.before("<h4 class='title'>" + title + "</h4>");
        var dTbl = target.DataTable({
            "data": table,
            "columns": headers
        });

        $(dTbl.table().header()).prepend(extraHeaders);
        target.dataTable().fnDraw();
    }
    
    function createDatasetLink(containerId,title,table) {
      
    }

    function incRatesTab(result) {
        // result has tables, headers and graphs properties
        var headers = createOutputHeaders([{
            data: "_row",
            title: "Age Group"
        }],result.headers);

        $("#rateTables .title").html("");
        var extraHeaders = "<tr role='row'><th>Rate</th><th colspan='" + (headers.length-1) + "'>Calendar Period</th></tr>";

        if (result.tables[0]) {
            createOutputTable("#rateTable1",model.titleA,result.tables[0],headers,extraHeaders);
            createGraphImage("#rateGraphs",result.graphs[0]);
        }

        if (result.tables[1]) {
            createOutputTable("#rateTable2",model.titleB,result.tables[1],headers,extraHeaders);
            createGraphImage("#rateGraphs",result.graphs[1]);
        }
    }

    function incRateRatioTab(result) {
        // result has tables, headers and graphs properties
        var headers = createOutputHeaders([{
            data: "_row",
            title: "Age Group"
        }],result.headers);

        $("#irrTables .title").html("");
        var extraHeaders = "<tr role='row'><th>Rate</th><th colspan='" + (headers.length-1) + "'>Calendar Period</th></tr>";
        
        if (result.tables[0]) {
            createOutputTable("#irrTable",(model.titleA + " vs " + model.titleB),result.tables[0],headers,extraHeaders);
            $("#irrGraphs").append(
                "<img class='img-responsive show' src='" + result.graphs[0][0] + "' />" +
                "<img class='img-responsive' src='" + result.graphs[1][0] + "' />");
            $("#showNumber").addClass("show");
        }
    }
    
    function apcRatesTab(result) {
        $("#local, #collapseOne > div").empty();
        if (result.AdjustedRates) {
            var ar = result.AdjustedRates;
            if (ar.ComparisonOfAdjustedRates) {
                var coar = ar.ComparisonOfAdjustedRates;
                var headers = createOutputHeaders([{
                    data: "_row",
                    title: "Null Hypothesis"
                }],coar.headers);
                if (coar.graphs[0]) {
                    createGraphImage("#collapseOne > div",coar.graphs[0][0]);
                }
                if (coar.tables[0]) {
                    $("#collapseOne > div").append('<div class="col-sm-6"><table id="coarTable" class="data-table stripe compact" width="100%"></table></div>');
                    createOutputTable("#coarTable",(model.titleA + " vs " + model.titleB),coar.tables[0],headers);
                }
            }
            if (ar.CrossSectionalAgeCurve) {
                var csac = ar.CrossSectionalAgeCurve;
                if (csac.graphs[0]) {
                    
                }
            }
            if (ar.FittedCohortPattern) {
                var fcp = ar.FittedCohortPattern;
                if (fcp.graphs[0]) {
                    
                }
            }
            if (ar.FittedTemporalTrends) {
                var ftt = ar.FittedTemporalTrends;
                if (ftt.graphs[0]) {
                    
                }
            }
        }
        if (result.LocalDrifts) {
            for (index in result.LocalDrifts.graphs) {
                createGraphImage("#local",result.LocalDrifts.graphs[index][0]);
            }
        }
    }


    function drop(e) {
        ReadFile.createModel({
            "id": $(e.delegateTarget).attr('data-target'),
            "files": e.originalEvent.dataTransfer.files
        }, updateModel);
    }

    /* ---------- Updates the UI and Model ---------- */
    function update() {
        var self = this;

        // Updates the model based on the contents of the file
        if (self.type == 'file') {
            ReadFile.createModel(self, updateModel);
            var target = $('#inputfile1, #inputfile2');
            target.wrap("<form>").closest("form")[0].reset();
            target.unwrap();
        } else {
            syncModel();
        }
    }

    function reset(e) {
        if (e !== undefined) e.preventDefault();
        $(".graphsContainers").empty();
        $('#ratePane').attr('style', 'display:none');
        $('#showNumber').removeClass('show');
        $('#showPlot').attr('style', 'display:none');
        $('#apcRatePane').attr('style', 'display:none');
        $('#apcRatioPane').attr('style', 'display:none');
        $('#description,#startAge,#startYear,#interval,#title1,#title2').val("");
        $(".tablesContainers table").each(function () {
            if ($.fn.DataTable.isDataTable(this)) {
                $(this).DataTable().destroy();
            }
            $(this).empty();
            $("h4.title").empty();
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
    };

    function table_input(target, data) {
        var target = $(target);
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
        var table = createInputTable("#" + target.prop("id"), createHeaders((width - 1) / 2, tableData), tableData).children('thead');
        if (self.model.startYear && self.model.interval) {
            var headerRow = $('<tr><th class="white-border"></th></tr>');
            for (var i = 0; i < (width - 1) / 2; i++) {
                var header = self.model.startYear + self.model.interval * i;
                headerRow.append($('<th class="header" colspan="2"></th>').html(header + "-" + (header + self.model.interval - 1)));
            }
            table.prepend(headerRow);
        }
        if (data.title && self.model.description) table.prepend('<tr><th class="white-border"></th><th class="header" colspan="' + (width - 1) + '">' + data.title + '<br/><span class="blue">' + self.model.description + '</span></th></tr>');
        target.children(".paste-here").remove();
    };

    /* ---------- Saves the file contents to the model ---------- */
    function updateModel(id, contents) {
        self.model[id] = contents;

        self.cfg.description.val(self.model[id].description);
        self.cfg.startAge.val(self.model[id].startAge);
        self.cfg.startYear.val(self.model[id].startYear);
        self.cfg.interval.val(self.model[id].interval);

        if (self.model.inputfile1)
            self.cfg.titleA.val(self.model.inputfile1.title);

        if (self.model.inputfile2)
            self.cfg.titleB.val(self.model.inputfile2.title);

        syncModel();
    }

    /* ---------- Saves DOM references in the configuration ---------- */
    function config(cfg) {
        self.cfg = cfg;
        return self.cfg;
    }

    /* ---------- Logs the current configuration and model to the console ---------- */
    function log() {
        console.log('Configuration', self.cfg);
        console.log('Model', self.model);
    }

    function createHeaders(length, data) {
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

    function createInputTable(containerID, headers, data) {
        var table = $(document.createElement('table'));
        table.attr('class', 'table display compact');
        table.attr('width', '100%');
        $(containerID).children(".dataTables_wrapper").children(".dataTable").DataTable().destroy();
        $(containerID).children("table").remove();
        $(containerID).prepend(table);
        table.DataTable({
            "destroy": true,
            "data": data,
            "columns": headers,
            "bSort": false,
            "bFilter": false,
            "paging": false,
            "responsive": true,
            "dom": 't',
            "scrollX": false
        });
        $(containerID).find('#inputTable_wrapper').addClass('table-responsive');
        $(containerID).find('.table').addClass('input-table');
        return table;
    };
})($, readfile);
