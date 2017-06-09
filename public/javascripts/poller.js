

function poll(envelopeId, fileId, protocolHostName) {
   setTimeout(function() {
      $.ajax({ url: protocolHostName+"/country-by-country-reporting/file-upload-response/"+envelopeId+"/"+fileId,
      success: function(data, statusText, xhr){
          if(xhr.status == 202) {
            window.location.href=protocolHostName+"/country-by-country-reporting/file-upload-ready/"+envelopeId + "/" + fileId
        }else{
          //Setup the next poll recursively
          poll(envelopeId, fileId, protocolHostName);
        }
      },
      error: function(xhr,status,error){
        var errMsg = xhr.responseText;
        console.log("Error status: " + status);
        console.log("Error message: " + errMsg);
        window.location.href= protocolHostName+"/country-by-country-reporting/technical-difficulties";
      }

      });
  }, 3000);
}
