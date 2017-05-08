

function poll(envelopeId, fileId, protocolHostName) {
   setTimeout(function() {
      $.ajax({ url: protocolHostName+"/country-by-country-reporting/getFileuploadResponse/"+envelopeId+"/"+fileId,
      success: function(data, statusText, xhr){
          if(xhr.status == 202) {
            var fileDetails = JSON.parse(xhr.responseText);
            window.location.href= protocolHostName+"/country-by-country-reporting/successFileUpload?fileName="+fileDetails.fileName+"&fileSize="+fileDetails.size;
        }else{
        //Setup the next poll recursively
        poll(envelopeId, fileId, protocolHostName);
        }
      },
      error: function(xhr,status,error){
      var errMsg = xhr.responseText;
        window.location.href= protocolHostName+"/country-by-country-reporting/errorFileUpload?errorMessage="+errMsg;
      }

      });
  }, 3000);
}