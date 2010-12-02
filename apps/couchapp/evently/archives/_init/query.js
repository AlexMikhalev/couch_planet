function() {
  var ret = {}

  var pathname = document.location.pathname
  var viewname = pathname.substring(pathname.lastIndexOf("/") + 1)
  if (viewname == "recent-posts-by-topic")
    ret.view = "archives-by-topic"
  else
    ret.view = "archives"

  var descending = $(document).getUrlParam("descending")
  var startkey = $(document).getUrlParam("startkey")
  var endkey = $(document).getUrlParam("endkey")
  if (descending)
    ret.descending = descending
  if (ret.view == "archives-by-topic") {
    if (startkey)
      ret.startkey = [JSON.parse(unescape(startkey))[0], "9999-12-31T23:59:59.999Z"]
    if (endkey)
      ret.endkey = [JSON.parse(unescape(endkey))[0], "0000-01-01T00:00:00.000Z"]
  }

  return ret
}
