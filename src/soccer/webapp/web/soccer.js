"use strict";

$(function () {

    // Restriction. // TODO - adjust this file extenstion restriction.
    $('#fileSelect').attr('accept', '.csv, text/plain');   

    // Initilize  
    init();
    function init()
    {
        var parameters = location.search.substring(1).split("&");
        console.log(parameters[0]);
        if(parameters[0]) {
            var temp = parameters[0].split("=");
            var fileId = unescape(temp[1]);             
            activateSoccerTab();
            showMetadata(fileId);
            showResult(fileId);              
        }        
        else {
            init_state();
        } 
    }

    // Initilize page state. 
    function init_state() {
        $('#progressBar').css('width', '0%');
        // Hide email Form.
        $('#emailForm').hide();
        // Hide calc Form. 
        $('#calcForm').hide();
        // Hide *ResultDiv.
        $('#resultDiv').hide();
        // Hide *QueueResultdiv
        $('#queueResultDiv').hide();
        // Clean file meta div
        $('#fileMetaDiv').text('');
        // hide validation Error Area.
        $('#validationErrorArea').hide();
        // Enable upload button if disabled.
        $('#fileSubmit').removeAttr('disabled');
        // Enable email submit button if disabled.
        $('#emailSubmit').removeAttr('disabled');
        // Enable calc submit button if disabled.
        $('#calcSubmit').removeAttr('disabled');
        // Hide result portion.
        $("#resultArea").hide();        
    }

    /*
     * Bind an event handler to the "change" JavaScript event, 
     * or trigger that event on an element.
     */
    $('#fileSelect').change(function () {
        console.log("fileSelect.onchange() invoked.");
        init_state();

        // Retrieve metadata of file.
        var file = document.getElementById('fileSelect').files[0];
        if (file) {
            var fileSize = 0;
            if (file.size > 1048576)
                fileSize = (Math.ceil(file.size / 1048576)).toString() + ' MB';
            else if (file.size > 1024)
                fileSize = (Math.ceil(file.size / 1024)).toString() + ' KB';
            else
                fileSize = file.size + ' Bytes';

            // Display meta data of the file.
            $('#fileMetaDiv').html("<i><strong>File Name: " + file.name + ";    File Size: " + fileSize + "</strong></i>");
            // Show progress bar div
            $('#progressDiv').show();
            $('#progressBar').text('0%');
            // Hide *ResultDiv.
            $('#resultDiv').hide();
            // Hide *QueueResultdiv
            $('#queueResultDiv').hide();
        }
    });

    /*
     * Bind an event handler to the “submit” JavaScript event, 
     * or trigger that event on an element.
     */
    $('#fileForm').submit(function (event) {
        console.log("fileForm.submit() invoked.");        
        event.preventDefault();

        // hide upload button.        
        $('#fileSubmit').attr('disabled', 'disabled');

        // Prepare formData and submit data.
        var formData = new FormData();
        formData.append("fileSelect", document.getElementById("fileSelect").files[0]);
        var xhr = new XMLHttpRequest();
        xhr.upload.addEventListener("progress", uploadProgress, false);
        xhr.addEventListener("load", uploadComplete, false);
        xhr.addEventListener("error", uploadFailed, false);
        xhr.addEventListener("abort", uploadCanceled, false);
        xhr.open("POST", "upload");
        xhr.send(formData);

        // Stay on the same page.
        return false;
    });

    /* 
     * Define callback function uploadProgress
     * @param {type} evt
     * @returns {undefined}
     */
    function uploadProgress(event) {
        if (event.lengthComputable) {
            var percentComplete = Math.round(event.loaded * 100 / event.total);
            $('#progressBar').css('width', percentComplete + '%');
            $('#progressBar').text(percentComplete.toString() + "%");
        }
        else {
            $("#progressBar").text("Uploading...");
        }
    }

    /*
     * Define callback function uploadComplete.
     * @param {type} evt
     * @returns {undefined}
     */
    function uploadComplete(event) {
        /* This event is raised when the server send back a response */
        console.log('uploadComplete() invoked. Response Text: ' + event.target.responseText);
        var responseText = event.target.responseText;

        // Hide progressDiv
        //$('#progressDiv').hide();

        var obj = $.parseJSON(responseText);
        console.log(obj.status);        
        $('#resultDiv').show();
        if (obj.status === 'pass') {
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-success');
            // set inputFileId Field value
            $('#inputFileId').attr('value', obj.inputFileId);
             // Show the calc Form and enable if disabled.
            $('#calcForm').show();
            $('#calcSubmit').removeAttr('disabled');
            $('#resultDiv').empty().append('Your file has been uploaded successfully.<br>Estimated processing time: <b>'+obj.estimatedTime+' seconds</b>');             
        }
        else if (obj.status === 'queue') {
            // Display continue message. 
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-warning');
            $('#resultDiv').empty().append('Your file has been uploaded successfully and is ready for processing. <br><br>'
                +'Estimated processing time: <b>'+obj.estimatedTime+' seconds</b> <br><br>'
                + 'We would like you to provide us with your email address, once we finish processing your file, you will get an email notification.');
            // set inputFileId Field value
            $('#inputFileId').attr('value', obj.inputFileId);
            // Show the email Form and enable if disabled.
            $('#emailForm').show();
            $('#emailForm').removeAttr('disabled');
        }
        else if (obj.status === 'invalid') {
            $('#resultDiv').hide();
            $('#validationErrorArea').show();
            $('#validationErrorArea').removeClass();
            $('#validationErrorArea').addClass('alert alert-warning');
            // refine resultDiv by adding validation errors.
            $('#validationErrorArea').empty().append('<b>Your file has been uploaded successfully but contains the following errors:</b><ul>');
            var arr = obj.details;
            console.log(arr);
            for (var i = 0; i < arr.length; i++)
                $('#validationErrorArea').append('<li>' + arr[i] + '</li>');
            $('#validationErrorArea').append('</ul>');
            $('#validationErrorArea').append('<br><b>Please modify your data file and re-upload.</b>');
        }
        else if (obj.status === 'fail') {
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-danger');
            // refine resultDiv by adding validation errors.
            $('#resultDiv').empty().append('<b>' + obj.errorMessage + '</b>');            
        }
        else {
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-danger');
            // refine resultDiv by adding validation errors.
            $('#resultDiv').empty().append('<b>Oops! Internal error.</b>');   
        }
    }

    /*
     * @description Handle upload failure event.
     * @param {type} event
     * @returns {undefined}
     */
    function uploadFailed(event) {
        alert("There was an error attempting to upload the file.");
    }

    /*
     * @description Handle upload cancellation event.
     * @param {type} event
     * @returns {undefined}
     */
    function uploadCanceled(event) {
        alert("The upload has been canceled by the user or the browser dropped the connection.");
    }

    /*
     * Given fileId, show the content.
     */
    function showResult(fileId) {
        var fileUrl = "files/" + fileId;
        var xmlHttpResult = new XMLHttpRequest();
        xmlHttpResult.open("GET", fileUrl, false);
        xmlHttpResult.send(null);    
        if(xmlHttpResult.status == 404) {
            $("#resultContent").text('The requested resource is not available.');
            $("#downloadHref1").text('');  
            $("#downloadHref2").text('');
        }
        else {
            $("#resultArea").show();
            $("#resultContent").text(xmlHttpResult.responseText);
            $("#downloadHref1").prop("href", fileUrl);   
            $("#downloadHref2").prop("href", fileUrl);          
        }
    }

    /*
     * Given fileId, show the metadata.
     */
    function showMetadata(fileId) {
        var fileUrl1 = "files/" + fileId + '.json';
        console.log(fileUrl1);
        var xmlHttpResult1 = new XMLHttpRequest();
        xmlHttpResult1.open("GET", fileUrl1, false);
        xmlHttpResult1.send(null);    
        if(xmlHttpResult1.status != 404) {           
            var obj1 = $.parseJSON(xmlHttpResult1.responseText);

            if(obj1.fileName) {
                var fileUrl2 = "files/" + obj1.fileName + '.json';
                console.log(fileUrl2);
                var xmlHttpResult2 = new XMLHttpRequest();
                xmlHttpResult2.open("GET", fileUrl2, false);
                xmlHttpResult2.send(null);    
                if(xmlHttpResult2.status == 404) {
                   ; // do nothing.
                }
                else {
                    var obj2 = $.parseJSON(xmlHttpResult2.responseText);                    

                    $('#queueResultDiv').removeClass();
                    $('#queueResultDiv').addClass('alert alert-success');   
                    $('#queueResultDiv').empty().append('Your file has been processed successfully.<br>'
                        + '<br>File Name: ' + obj2.fileName 
                        + '<br>File Size: ' + obj2.fileSize + ' Bytes'                        
                        + '<br>Processing Time: ' + obj2.estimatedTime  + ' Seconds'
                        + '<br>Uploaded on: ' + obj1.timeStamp);
                    $('#queueResultDiv').show();
                }      
            }      
        }
    }

    /*
     * Bind an event handler to the “emailSubmit” JavaScript event, 
     * or trigger that event on an element.
     */
    $('#emailForm').submit(function (event) {
        console.log("emailForm.submit() invoked.");
        event.preventDefault();
       
        // Hide emailForm
        $('#emailForm').hide();

        // Prepare formData and submit data.
        var emailFormData = new FormData();
        emailFormData.append("emailAddress", $('#emailAddress').val());
        emailFormData.append("inputFileId", $('#inputFileId').val());
        var xhr = new XMLHttpRequest();
        xhr.addEventListener("load", function (event) {
            /* This event is raised when the server send back a response */
            console.log('emailForm load is completed. Response Text: ' + event.target.responseText);
            var responseText = event.target.responseText;
            
            var obj = $.parseJSON(responseText);
            console.log(obj.status);            
            $('#queueResultDiv').show();
            if (obj.status === 'pass') {
                // Change result Div to success status.
                $('#resultDiv').removeClass();
                $('#resultDiv').addClass('alert alert-success');
                $('#resultDiv').hide();
                // Set ququeResultDiv to success status.
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-success');
                $('#queueResultDiv').empty().append('Congratulations! Your file has been added into our queue successfully! '
                    + 'Once the file is processed, you will get an email notification at <b>' + obj.emailAddress 
                    + '</b>. <br><br>You can close this window or upload other files now.'); 
            } else if (obj.status === 'fail') {
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-danger');
                // refine queueResultDiv by adding validation errors.
                $('#queueResultDiv').empty().append('<b>' + obj.errorMessage + '</b>');            
            }
            else {
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-danger');
                // refine resultDiv by adding validation errors.
                $('#queueResultDiv').empty().append('<b>Oops! Internal error.</b>');   
            }
        }, false);
        xhr.addEventListener("error", uploadFailed, false);
        xhr.addEventListener("abort", uploadCanceled, false);
        xhr.open("POST", "queue");
        xhr.send(emailFormData);

        // Stay on the same page.
        return false;
    });

    /*
     * Bind an event handler to the “calcSubmit” JavaScript event, 
     * or trigger that event on an element.
     */
    $('#calcForm').submit(function (event) {
        console.log("calcForm.submit() invoked.");
        event.preventDefault();

        // Hide calcForm
        $('#calcForm').hide(); 

        $('#queueResultDiv').show();
        $('#queueResultDiv').html('<img width="28px" src="processing.gif">processing...');  

        // Prepare formData and submit data.
        var calcFormData = new FormData();       
        calcFormData.append("inputFileId", $('#inputFileId').val());
        var xhr = new XMLHttpRequest();
        xhr.addEventListener("load", function (event) {
            /* This event is raised when the server send back a response */
            console.log('calcForm load is completed. Response Text: ' + event.target.responseText);
            var responseText = event.target.responseText;

            var obj = $.parseJSON(responseText);
            console.log(obj.status);             
            $('#queueResultDiv').show();
            if (obj.status === 'pass') {              
                // Set ququeResultDiv to success status.
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-success');
                $('#queueResultDiv').empty().append('Your file has been processed successfully.');  
                // show result
                showResult(obj.outputFileId);
            } else if (obj.status === 'fail') {
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-danger');
                // refine queueResultDiv by adding validation errors.
                $('#queueResultDiv').empty().append('<b>' + obj.errorMessage + '</b>');            
            }
            else {
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-danger');
                // refine resultDiv by adding validation errors.
                $('#queueResultDiv').empty().append('<b>Oops! Internal error.</b>');   
            }

        }, false);
        xhr.addEventListener("error", uploadFailed, false);
        xhr.addEventListener("abort", uploadCanceled, false);
        xhr.open("POST", "calc");
        xhr.send(calcFormData);

        // Stay on the same page.
        return false;
    });

    // Cancel actions.
    $('#cancelCalc').click(function (event) {        
        init_state(); 
    });
    $('#cancelSubmit').click(function (event) {        
        init_state(); 
    });

    // tab handling.
    $('#myTab a').click(function(e){
        e.preventDefault();        
        $(this).tab('show');
    });

    // Info button.  ----- TEMP
    $("#btnInfo").hover(function(e){
        var socSystem = $('#socSystem').val();
        //$(this).attr('data-original-title', $('#socSystem option:selected').text());         
        //$(this).attr('data-content', 'value: ' + socSystem);   
        $(this).attr('data-original-title', "MODEL: SOCcer");         
        $(this).attr('data-content', "Features: JOB Title, SIC, Job Tasks"); 
        $(this).popover('show');
    }, function() {
        $(this).popover('hide');
    });      

});

function activateSoccerTab() {  
    $('#myTab a[href="#soccerTab"]').tab('show');
}