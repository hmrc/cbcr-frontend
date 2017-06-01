

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
        window.location.href= protocolHostName+"/country-by-country-reporting/technical-difficulties";
      }

      });
  }, 3000);
}