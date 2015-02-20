"use strict";

$(function () {

    // Initilize  
    init();
    function init()
    {
        var parameters = location.search.substring(1).split("&");
        var temp = parameters[0].split("=");
        var fileId = unescape(temp[1]);
        var fileUrl = "files/" + fileId;

        var xmlHttp = new XMLHttpRequest();
        xmlHttp.open("GET", fileUrl, false);
        xmlHttp.send(null);    
        if(xmlHttp.status == 404) {
            $("#fileContent").text('The requested resource is not available.');
            $("#downloadHref1").text('');
            $("#downloadHref2").text('');
        }
        else {
            $("#fileContent").text(xmlHttp.responseText);
            $("#downloadHref1").prop("href", fileUrl);
            $("#downloadHref2").prop("href", fileUrl);
        }

    }

});