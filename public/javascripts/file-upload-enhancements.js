(function(document, window) {
    function poll(pollPath, readyPath, virusFailPath, errorPath) {
        window.fetch(pollPath)
            .then(function(response) {
                if (response.status === 202) {
                    window.location.href = readyPath
                } else if (response.status === 409) {
                    window.location.href = virusFailPath
                } else if (!response.ok) {
                    window.location.href = errorPath
                } else {
                    setTimeout(function () {
                        poll(pollPath, readyPath, virusFailPath, errorPath)
                    }, 3000)
                }
            })
            .catch(function(e) {
                console.error("Failed to reach the file upload path for checking status of upload", e.message)
            })
    }
    const progressIndicator = document.getElementById('file-upload-progress')
    if(progressIndicator) {
        const urls = progressIndicator.dataset
        poll(urls.poll, urls.success, urls.virus, urls.error)
    }

    function renderFormError(uploadInput) {
        const existingErrorSummary = document.querySelector(".govuk-error-summary")
        if(existingErrorSummary) {
            existingErrorSummary.focus()
            return true;
        }
        const errorPrefix = "Error: "
        if(document.title.substring(0, 7) !== errorPrefix) {
            document.title = errorPrefix + document.title
        }
        const summaryTpl = document.getElementById('empty-file-error-summary')
        const errorTpl = document.getElementById('empty-file-error-message')
        const mainContent = document.querySelector('#main-content > div > div')
        const formGroup = mainContent.querySelector(".govuk-form-group")
        mainContent.prepend(summaryTpl.content)
        const errorSummary = document.querySelector(".govuk-error-summary")
        formGroup.classList.add("govuk-form-group--error")
        formGroup.insertBefore(errorTpl.content, uploadInput)
        uploadInput.setAttribute("aria-describedby", "file-upload-error")
        uploadInput.classList.add("govuk-file-upload--error")
        errorSummary.focus()
    }

    const fileUploadForm = document.getElementById('fileUploadForm')
    if(fileUploadForm) {
        const uploadInput = document.getElementById('file-input')
        fileUploadForm.setAttribute("novalidate", "novalidate")
        fileUploadForm.addEventListener('submit', function(event) {
            event.preventDefault()
            if(uploadInput.value.trim() === "") {
                renderFormError(uploadInput)
            } else {
                this.submit()
            }
        })
    }
})(document, window)
