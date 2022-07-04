if(document.getElementById('file-input')){
    // file upload

    var form = document.getElementsByTagName('form')[0];
    var error_summary = document.getElementById('error-summary-display');
    var formgroup = document.getElementById('formGroup');
    var errorMessage = document.getElementById('error-message');
    var title = document.title;
    var fileInput = document.getElementById('file-input')

    // set a novalidate to remove the HTML5 validation when js is present (and the pattern error message/summary can be displayed)
    // pending handling of no file
    form.setAttribute('novalidate','novalidate');
    // remove required when js is present to prevent triggering HTML5 validation
    fileInput.removeAttribute('required');

    form.addEventListener('submit', checkFile);

    function checkFile(ev){
        if (fileInput.value === '') {
            ev.preventDefault();
            document.title = fileInput.getAttribute('data-page-title-prefix') + " " + title
            fileInput.setAttribute('aria-describedby', 'error-message');
            error_summary.classList.remove("js-hidden");
            formgroup.classList.add("govuk-form-group--error");
            errorMessage.classList.remove("js-hidden");
            error_summary.focus();
        }
    }
}