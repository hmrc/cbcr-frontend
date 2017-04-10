

function poll(envelopeId, protocolHostName) {
   setTimeout(function() {

      $.ajax({ url: protocolHostName+"/country-by-country-reporting/getFileuploadResponse/"+envelopeId, success: function(data){
        var key = "status";
        var value = data[key];
        var eKey = "envelopeId"
        var eVal = data[eKey]

        if(eVal == envelopeId && value == 'AVAILABLE'){
            window.location.href= protocolHostName+"/country-by-country-reporting/successFileUpload";
        } else{
        //Setup the next poll recursively
        poll(envelopeId, protocolHostName);
        }
      }, dataType: "json"});
  }, 2000);
}