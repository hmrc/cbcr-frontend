(async function(document, window) {
    const sleep = ms => new Promise(resolve => window.setTimeout(resolve, ms))
    const progressIndicator = document.getElementById('file-upload-progress')
    const config = progressIndicator && progressIndicator.dataset
    const pendingContent = document.getElementById('pending-content')
    const readyContent = document.getElementById('ready-content')
    const continueButtonWrapper = document.getElementById('continue-button-wrapper')
    if(progressIndicator && pendingContent) {
        while(progressIndicator.firstChild) {
            progressIndicator.removeChild(progressIndicator.firstChild)
        }
        progressIndicator.appendChild(pendingContent.content)
        // aria-live added only once we have our starting content in place
        progressIndicator.setAttribute('aria-live', 'assertive')
        progressIndicator.removeAttribute('class')
        await sleep(3000)
        await poll(config, 1)
    }

    async function poll(config, count) {
        if(count >= Number(config['maxPolls'])) {
            window.location.href = `${config.handleError}?errorCode=408&reason=timed-out)`
        } else {
            try {
                await fetchStatus(config, count)
            } catch(e) {
                window.location.href = config.error
            }
        }
    }

    async function fetchStatus(config, count) {
        const response = await window.fetch(config.poll)
        if (response.status === 202 || response.status === 409 || !response.ok) {
            // these outcomes are handled by the results page
            await fileIsReady()
        } else {
            await sleep(1000)
            await poll(config, count+1)
        }
    }

    async function fileIsReady() {
        while(progressIndicator.firstChild) {
            progressIndicator.removeChild(progressIndicator.firstChild)
        }
        // use setTimeout to ring-fence updates to the live region within the event loop
        window.setTimeout(function(){ progressIndicator.appendChild(readyContent.content) }, 10)
        // ensure the button is rendered after the content and not before it
        await sleep(20)
        progressIndicator.after(continueButtonWrapper.content)
    }

    function renderFormError(uploadInput) {
        const existingErrorSummary = document.querySelector('.govuk-error-summary')
        const submitButton = document.getElementById('upload-button')
        if(existingErrorSummary) {
            existingErrorSummary.focus()
            submitButton.removeAttribute('disabled')
            return;
        }
        const errorPrefix = 'Error: '
        if(document.title.substring(0, 7) !== errorPrefix) {
            document.title = errorPrefix + document.title
        }
        const summaryTpl = document.getElementById('empty-file-error-summary')
        const errorTpl = document.getElementById('empty-file-error-message')
        const mainContent = document.querySelector('#main-content > div > div')
        const formGroup = mainContent.querySelector('.govuk-form-group')
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
        const uploadInput = document.getElementById('file-input')
        uploadInput.removeAttribute('required')
        fileUploadForm.setAttribute('novalidate', 'novalidate')
        fileUploadForm.addEventListener('submit', function(event) {
            document.getElementById('upload-button').setAttribute('disabled', 'true')
            const files = uploadInput.files
            if(files.length === 0) {
                event.preventDefault()
                return renderFormError(uploadInput)
            } else if(files[0].type !== 'text/xml') {
                event.preventDefault()
                window.location.href = `${config.handleError}?errorCode=415&reason=file-type`
            } else if((files[0].size / (1024 * 1024)) > 50) {
                event.preventDefault()
                window.location.href = `${config.handleError}?errorCode=413&reason=too-large`
            }
        })
    }
})(document, window)
