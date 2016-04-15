$(function() {
  if (!document.getElementById('adamant-pastable')) {
    $.extend(true,$,{
      "adamant": {
        "pastable": {
          "functions": {}
        },
        //"location": "http://wobenshain.github.io/adamant-meow"
        "location": "."
       }
    });
    $('<link href="'+$.adamant.location+'/adamant-pastable/adamant-pastable.css" id="adamant-pastable" rel="stylesheet" type="text/css" />').prependTo(document.head);
    $('[adamant-pastable]').append('<label for="pastehere"></label>').append("<textarea></textarea>").on('paste','textarea',function(e) {
      setTimeout(function() {
        var pastefunction = $.adamant.pastable.functions[$(e.target).parent().attr('adamant-pastable')];
        var data = $(e.target).val();
        pastefunction($(e.target).parent(),data);
      },1);
    });
  }
});