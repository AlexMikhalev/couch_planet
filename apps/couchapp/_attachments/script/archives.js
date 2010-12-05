$(function() {
  new Archives('archiveYear', 'archiveMonth')
})

function Archives(yearClassName, monthClassName) {
  var ccn = 'clicker'
  var clcn = 'closed'
  var opcn = 'open'

  var toggleByClassName = function(cname) {
    clickers = $('.' + cname)
    if (clickers.length < 1)
      return
    // close togglers, but keep
    // the one for the current year open
    var i
    if (cname == yearClassName) {
      clickers[0].className += ' ' + ccn
      clickers[0].className += ' ' + opcn
      clickers[0].onclick = function() {
        toggleNextOpenClose(this)
      }
      i = 1
    } else {
      i = 0
    }
    for (; i < clickers.length; i++) {
      clickers[i].className += ' ' + ccn
      clickers[i].className += ' ' + clcn
      clickers[i].onclick = function() {
        toggleNextOpenClose(this)
      }
      toggleNextOpenClose(clickers[i])
    }
  }

  var toggleNextOpenClose = function(el) {
    var next = el.nextSibling
    el.className = el.className.replace(new RegExp(opcn + '\\b'), '')
    el.className = el.className.replace(new RegExp(clcn + '\\b'), '')
    while(next.nodeType != 1)
      next = next.nextSibling
    next.style.display = (next.style.display == 'none' ? 'block' : 'none')
    el.className += (next.style.display == 'block' ? ' ' + opcn : ' ' + clcn)
  }

  toggleByClassName(yearClassName)
  toggleByClassName(monthClassName)

  var as = $("#archives a")
  for (var i = 0; i < as.length; i++) {
    var startkey = $(document).getUrlParam("startkey")
    if (startkey && startkey == $(as[i]).getUrlParam("startkey")) {
      as[i].style.fontWeight = "bold"
      break
    }
  }
}
