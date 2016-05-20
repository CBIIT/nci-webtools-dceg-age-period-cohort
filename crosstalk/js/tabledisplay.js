$(function() {
    var tabledata = window.tabledata;
    var headers = tabledata.shift();
    var target = $("table");
    if ($.fn.DataTable.isDataTable("table")) {
        target.DataTable().destroy();
    }
    headers = headers.map(function(entry) {
        return {
            data: entry,
            title: entry
        };
    });
    target.DataTable({
        "data": tabledata,
        "columns": headers,
        "destroy": true,
        "bSort": false,
        "bFilter": false,
        "paging": false,
        "responsive": true,
        "dom": 't'
    });
    target.dataTable().fnDraw();
});