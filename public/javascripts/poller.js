

function poll(envelopeId, protocolHostName) {
   setTimeout(function() {
alert("polling call")
      $.ajax({ url: protocolHostName+"/country-by-country-reporting/getFileuploadResponse/"+envelopeId, success: function(data, statusText, xhr){
//        var key = "status";
//        var value = data[key];
//        var eKey = "envelopeId"
//        var eVal = data[eKey]

//        if(eVal == envelopeId && value == 'AVAILABLE'){
          if(xhr.status == '202') {
          alert("got the status: "+xhr.status)
            window.location.href= protocolHostName+"/country-by-country-reporting/successFileUpload";
        } else{
        //Setup the next poll recursively
        alert("polled again")
        poll(envelopeId, protocolHostName);
        }
      }, dataType: "json"});
  }, 4000);
}