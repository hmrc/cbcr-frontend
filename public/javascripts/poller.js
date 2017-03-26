

function poll(envelopeId) {
   setTimeout(function() {

      var devUrl = "http://www-dev.tax.service.gov.uk"
      var localUrl = "http://localhost:9696"

      $.ajax({ url: localUrl+"/country-by-country-reporting/getFileuploadResponse/"+envelopeId, success: function(data){
        var key = "status";
        var value = data[key];
        var eKey = "envelopeId"
        var eVal = data[eKey]

        if(eVal == envelopeId && value == 'AVAILABLE'){
            window.location.href= localUrl+"/country-by-country-reporting/successFileUpload";
        } else{
        //Setup the next poll recursively
        poll(envelopeId);
        }
      }, dataType: "json"});
  }, 2000);
}