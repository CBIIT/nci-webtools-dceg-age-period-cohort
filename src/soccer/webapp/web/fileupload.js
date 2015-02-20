"use strict";

$(function () {

    // Restriction. // TODO - adjust this file extenstion restriction.
    $('#fileSelect').attr('accept', '.csv, text/plain');

    // Initilize    
    init_state();
    function init_state() {
        $('#progressBar').css('width', '0%');
        // Hide email Form.
        $('#emailForm').hide();
        // Hide *ResultDiv.
        $('#resultDiv').hide();
        // Hide *QueueResultdiv
        $('#queueResultDiv').hide();
        // Clean file meta div
        $('#fileMetaDiv').text('');
        // Enable upload button if disabled.
        $('#fileSubmit').removeAttr('disabled');
        // Enable email submit button if disabled.
        $('#emailSubmit').removeAttr('disabled');
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
            $('#fileMetaDiv').html("<strong>File Name: <i>" + file.name + "</i>;    File Size: <i>" + fileSize + "</i></strong> ");
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
        $('#resultDiv').html('<b>' + obj.message + '</b>');
        $('#resultDiv').show();
        if (obj.status === 'pass') {
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-success');
            $('#resultDiv').empty().append('<b>' + obj.message + '</b><br><br>');
            $('#resultDiv').append('<a target="_blank" href="soccerouput.html?fileid=' + obj.outputFileUrl + '">Click here to view SOCcer output</a>.');
        }
        if (obj.status === 'queue') {
            // Display continue message. 
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-warning');
            // set inputFileId Field value
            $('#inputFileId').attr('value', obj.inputFileId);
            // Show the email Form and enable if disabled.
            $('#emailForm').show();
            $('#emailForm').removeAttr('disabled');
        }
        if (obj.status === 'invalid') {
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-warning');
            // refine resultDiv by adding validation errors.
            $('#resultDiv').empty().append('<b>' + obj.message + '</b><ul>');
            var arr = obj.details;
            console.log(arr);
            for (var i = 0; i < arr.length; i++)
                $('#resultDiv').append('<li>' + arr[i] + '</li>');
            $('#resultDiv').append('</ul>');
            $('#resultDiv').append('<br><b>Please modify your data file and re-upload.</b>');
        }
        if (obj.status === 'fail') {
            $('#resultDiv').removeClass();
            $('#resultDiv').addClass('alert alert-danger');
            // refine resultDiv by adding validation errors.
            $('#resultDiv').empty().append('<b>' + obj.message + '</b><ul>');            
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
     * Bind an event handler to the “emailSubmit” JavaScript event, 
     * or trigger that event on an element.
     */
    $('#emailForm').submit(function (event) {
        console.log("emailForm.submit() invoked.");
        event.preventDefault();

        // hide upload button.
        $('#emailSubmit').attr('disabled', 'disabled');

        // Prepare formData and submit data.
        var emailFormData = new FormData();
        emailFormData.append("emailAddress", $('#emailAddress').val());
        emailFormData.append("inputFileId", $('#inputFileId').val());
        var xhr = new XMLHttpRequest();
        xhr.addEventListener("load", function (event) {
            /* This event is raised when the server send back a response */
            console.log('emailForm load is completed. Response Text: ' + event.target.responseText);
            var responseText = event.target.responseText;

            // Hide emailForm
            $('#emailForm').hide();

            var obj = $.parseJSON(responseText);
            console.log(obj.status);
            $('#queueResultDiv').html('<b>' + obj.message + '</b>');
            $('#queueResultDiv').show();
            if (obj.status === 'pass') {
                // Change result Div to success status.
                $('#resultDiv').removeClass();
                $('#resultDiv').addClass('alert alert-success');
                // Set ququeResultDiv to success status.
                $('#queueResultDiv').removeClass();
                $('#queueResultDiv').addClass('alert alert-success');
                $('#queueResultDiv').empty().append('<b>' + obj.message + '</b><br><br>');
            }
        }, false);
        xhr.addEventListener("error", uploadFailed, false);
        xhr.addEventListener("abort", uploadCanceled, false);
        xhr.open("POST", "queue");
        xhr.send(emailFormData);

        // Stay on the same page.
        return false;
    });

});