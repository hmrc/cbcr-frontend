

function poll(envelopeId) {
   setTimeout(function() {
      $.ajax({ url: "http://localhost:9000/cbcr-frontend/getFileuploadResponse/"+envelopeId, success: function(data){
        var key = "status";
        var value = data[key];
        var eKey = "envelopeId"
        var eVal = data[eKey]

        if(eVal == envelopeId && value == 'AVAILABLE'){
            window.location.href="http://localhost:9000/cbcr-frontend/successFileUpload";
        } else{
        //Setup the next poll recursively
        poll(envelopeId);
        }
      }, dataType: "json"});
  }, 1000);
}