function poll(pollPath, readyPath, virusFailPath, badRequestPath, errorPath) {
    setTimeout(function() {
        $.ajax({ url: pollPath,
            success: function(data, statusText, xhr){
                if(xhr.status == 202) {
                    window.location.href=readyPath
                }else{
                    //Setup the next poll recursively
                    poll(pollPath, readyPath, virusFailPath, errorPath);
                }
            },
            error: function(xhr,status,error){
                var errMsg = xhr.responseText;
                if(xhr.status == 409) {
                    window.location.href=virusFailPath;
                } else if(xhr.status == 400) {
                    window.location.href=badRequestPath;
                } else {
                    window.location.href=errorPath;
                }
            }

        });
    }, 3000);
}