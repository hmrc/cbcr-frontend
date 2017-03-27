

function poll(envelopeId) {
   setTimeout(function() {
      $.ajax({ url: "https://www-dev.***REMOVED***/country-by-country-reporting/getFileuploadResponse/"+envelopeId, success: function(data){
        var key = "status";
        var value = data[key];
        var eKey = "envelopeId"
        var eVal = data[eKey]

        if(eVal == envelopeId && value == 'AVAILABLE'){
            window.location.href="https://www-dev.***REMOVED***/country-by-country-reporting/successFileUpload";
        } else{
        //Setup the next poll recursively
        poll(envelopeId);
        }
      }, dataType: "json"});
  }, 2000);
}