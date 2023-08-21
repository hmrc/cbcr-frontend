(function(document, window) {
    function poll(urls) {
        window.fetch(urls.poll)
            .then(function(response) {
                const status = response.status
                if (status === 202) {
                    window.location.href = urls.success
                } else if (status === 409) {
                    window.location.href = urls.virus
                } else if (!response.ok) {
                    window.location.href = urls.error
                } else {
                    setTimeout(function () {
                        poll(urls)
                    }, 3000)
                }
            })
            .catch(function(e) {
                console.error('Failed to reach the file upload path for checking status of upload', e.message)
                window.location.href = `${urls.handleError}?errorCode=${e.status}&reason=${e.message})`
            })
    }

    function renderFormError(uploadInput) {
        const existingErrorSummary = document.querySelector('.govuk-error-summary')
        if(existingErrorSummary) {
            existingErrorSummary.focus()
            return true;
        }
        const errorPrefix = 'Error: '
        if(document.title.substring(0, 7) !== errorPrefix) {
            document.title = errorPrefix + document.title
        }
        const summaryTpl = document.getElementById('empty-file-error-summary')
        const errorTpl = document.getElementById('empty-file-error-message')
        const mainContent = document.querySelector('#main-content > div > div')
        const formGroup = mainContent.querySelector('.govuk-form-group')
        const submitButton = document.getElementById('upload-button')
        mainContent.prepend(summaryTpl.content)
        const errorSummary = document.querySelector('.govuk-error-summary')
        formGroup.classList.add('govuk-form-group--error')
        formGroup.insertBefore(errorTpl.content, uploadInput)
        uploadInput.setAttribute('aria-describedby', 'file-input-hint file-upload-error')
        uploadInput.classList.add('govuk-file-upload--error')
        errorSummary.focus()
        submitButton.removeAttribute('disabled')
    }

    const fileUploadForm = document.getElementById('fileUploadForm')
    if(fileUploadForm) {
        const progressIndicator = document.getElementById('file-upload-progress')
        const urls = progressIndicator.dataset
        const uploadInput = document.getElementById('file-input')
        const liveRegionContent = document.getElementById('live-region-content')
        uploadInput.removeAttribute('required')
        fileUploadForm.setAttribute('novalidate', 'novalidate')
        fileUploadForm.addEventListener('submit', function(event) {
            event.preventDefault()
            document.getElementById('upload-button').setAttribute('disabled', 'true')
            progressIndicator.classList.add("active")
            const files = uploadInput.files
            if(files.length === 0) {
                renderFormError(uploadInput)
            } else if(files[0].type !== 'text/xml') {
                window.location.href = `${urls.handleError}?errorCode=415&reason=file-type`
            } else if((files[0].size / (1024 * 1024)) > 50) {
                window.location.href = `${urls.handleError}?errorCode=413&reason=too-large`
            } else {
                progressIndicator.appendChild(liveRegionContent.content)
                let formData = new FormData();
                formData.append('file', files[0]);
                window
                    .fetch(fileUploadForm.dataset.withPolling, {
                        withCredentials: true,
                        method: 'post',
                        body: formData,
                        mode: 'no-cors'
                    })
                    .then((r) => {
                        return poll(urls)
                    })
                    .catch( error => {
                        window.location.href = `${urls.handleError}?errorCode=${error.status}&reason=${error.message}`
                    })
            }
        })
    }
})(document, window)