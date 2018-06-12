/*
 * Script adapted from timeout-dialog.js v1.0.1, 01-03-2012
 *
 * @author: Rodrigo Neri (@rigoneri)
 *
 * (The MIT License)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

Date.now = Date.now || function() { return +new Date; };
 if (!window.console){ console = {log: function() {}} };

function secondsToTime (secs) {
  var hours = Math.floor(secs / (60 * 60))

  var divisorForMinutes = secs % (60 * 60)
  var minutes = Math.floor(divisorForMinutes / 60)

  var divisorForSeconds = divisorForMinutes % 60
  var seconds = Math.ceil(divisorForSeconds)

  var obj = {
    'h': hours,
    'm': minutes,
    's': seconds
  }
  return obj
}

String.prototype.format = function () {
  var s = this
  var i = arguments.length

  while (i--) {
    s = s.replace(new RegExp('\\{' + i + '\\}', 'gm'), arguments[i])
  }
  return s
}

!function ($) {
  $.timeoutDialog = function (options) {
    var settings = {
    }

    $.extend(settings, options)

    var TimeoutDialog = {
      init: function () {
        this.setupDialogTimer()
      },

      setupDialogTimer: function () {
        var self = this
        self.dialogOpen = false
        window.setTimeout(function () {
          self.setupDialog()
        }, ((settings.timeout) - (settings.countdown)) * 1000)
      },

      setupDialog: function () {
        // check this
        var self = this
        self.dialogOpen = true
        self.startTime = Math.round(Date.now()/1000, 0);
        self.currentMin = Math.ceil(settings.timeout / 60)
        self.destroyDialog()
          $('html').addClass('noScroll')
        var time = secondsToTime(settings.countdown)
        var timeout = secondsToTime(settings.timeout)
         // AL: Add live region and edit dialog structure
         $('<div id="timeout-dialog" class="timeout-dialog" role="dialog" aria-labelledby="timeout-heading" aria-live="assertive">' +
                    '<h2 id="timeout-heading" class="heading-medium">' + settings.title + '</h2>' +
                    '<p id="timeout-message">' +
                    settings.message + ' <br /><span class="countdown" id="timeout-countdown" role="text">' + time.m + ' ' + settings.messageMinutes + '</span>' +
                    '</p>' +
                    '<button id="timeout-keep-signin-btn" class="button">' + settings.keep_alive_button_text.format(settings.timeout / 60) + '</button>' +
                    '</div>' +
                    '<div id="timeout-overlay" class="timeout-overlay"></div>')
                    .appendTo('body');

                self.activeElement = document.activeElement;

                $('#timeout-keep-signin-btn').focus();

                // AL: disable the non-dialog page to hide un-navigable accessibility tree
                // can be removed once aria-modal is fully supported
        $('#skiplink-container, body>header, #global-cookie-message, body>main, body>footer').attr('aria-hidden', 'true')

        self.addEvents();

        self.startCountdown(settings.countdown)

        self.escPress = function (event) {
          if (self.dialogOpen && event.keyCode === 27) {
                        self.closeDialog()
          }
        }

        self.closeDialog = function () {
          if (self.dialogOpen) {
            self.keepAlive()
            self.activeElement.focus()
          }
        }

        // AL: prevent scrolling on touch, but allow pinch zoom
        self.handleTouch = function(e){
            var touches = e.originalEvent.touches || e.originalEvent.changedTouches;
            if ($('#timeout-dialog').length) {
                if(touches.length == 1){
                    e.preventDefault();
                }
            }
        }
        // AL: add touchmove handler
        $(document).on('touchmove', self.handleTouch)
        $(document).on('keydown', self.escPress)
        $('#timeout-keep-signin-btn').on('click', self.closeDialog)
      },

      destroyDialog: function () {
        $(document).off('touchmove', self.handleTouch)
        if ($('#timeout-dialog').length) {
          self.dialogOpen = false
          $('.timeout-overlay').remove()
          $('#timeout-dialog').remove()
          $('html').removeClass('noScroll')
        }

        // AL: re-enable the non-dialog page
        $('#skiplink-container, body>header, #global-cookie-message, body>main, body>footer').removeAttr('aria-hidden')
      },

      // AL: moved updater to own call to allow calling from other events
      updateUI: function(counter){
        var self = this
        if (counter < 60) {
          if(counter < 0){
            counter = 0;
          }
            $("#timeout-dialog").removeAttr('aria-live');
            $('#timeout-countdown').html(counter + " " + settings.messageSeconds)
        } else {
          var newCounter = Math.ceil(counter / 60);
          if(newCounter < self.currentMin){
              self.currentMin = newCounter
              var newMessage = "";
              if(newCounter == 1){
                 newMessage = newCounter + " " + settings.messageMinute
              } else if(newCounter > 1 && newCounter < 3) {
                 newMessage = newCounter + " " + settings.messageMinutesTwo
              } else {
                 newMessage = newCounter + " " + settings.messageMinutes
              }
                 $('#timeout-countdown').html(newMessage)
          }
        }
      },

      addEvents: function(){
        var self = this;

        // trap focus in modal (or browser chrome)
        $('a, input, textarea, button, [tabindex]').not('[tabindex="-1"]').on("focus", function (event) {
            var modalFocus = document.getElementById("timeout-dialog")
            if(modalFocus && self.dialogOpen){
                if(!modalFocus.contains(event.target)) {
                    event.stopPropagation()
                    modalFocus.focus()
                }
            }
        });

        function handleFocus(){
            if(self.dialogOpen){
                // clear the countdown
                window.clearInterval(self.countdown)
                // calculate remaining time
                var now = Math.round(Date.now()/1000, 0)
                var expiredSeconds = now - self.startTime;
                var currentCounter = settings.countdown - expiredSeconds;
                self.updateUI(currentCounter);
                self.startCountdown(currentCounter);
                $('#timeout-keep-signin-btn').focus();
            }
        }

        // AL: handle browsers pausing timeouts/intervals by recalculating the remaining time on window focus
        // need to widen this to cover the setTimeout which triggers the dialog for browsers which pause timers on blur
        // hiding this from IE8 as it breaks the reset - to investigate further
        if (navigator.userAgent.match(/MSIE 8/) == null) {
                $(window).off("focus", handleFocus);
                $(window).on("focus", handleFocus);
        }
      },
      startCountdown: function (counter) {
        var self = this
        self.countdown = window.setInterval(function () {
          counter -= 1

          self.updateUI(counter);

          if (counter <= 0) {
            self.signOut()
          }
        }, 1000)
      },

      keepAlive: function () {
        var self = this
        self.destroyDialog()
        window.clearInterval(this.countdown)
        $(document).off('keydown', self.escPress)

        $.get(settings.keep_alive_url, function (data) {
          if (data === 'OK') {
              self.setupDialogTimer()
            }
          else {
            self.signOut()
          }
        })
      },

      signOut: function () {
        var self = this
        window.clearInterval(self.countdown)
        self.destroyDialog()
        window.location = settings.logout_url

       }

    }

    TimeoutDialog.init()
  }
}(window.jQuery)