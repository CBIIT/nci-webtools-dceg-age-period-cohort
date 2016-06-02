$(function() {
    function writeTable(containerID,data) {
        var headers = data.shift();
        var target = $(containerID);
        if ($.fn.DataTable.isDataTable(containerID)) {
            target.DataTable().destroy();
        }
        headers = headers.map(function(entry) {
            return {
                data: entry,
                title: entry
            };
        });
        target.DataTable({
            "data": data,
            "columns": headers,
            "destroy": true,
            "bSort": false,
            "bFilter": false,
            "paging": false,
            "responsive": true,
            "dom": 't'
        });
        target.dataTable().fnDraw();
    }
    var tabledata = window.opener.requestTabledata();
    for (var index in tabledata) {
        $('body').append('<table class="table display compact" width="100%"></table>');
        writeTable('body > *:last-child',tabledata[index]);
    }
});