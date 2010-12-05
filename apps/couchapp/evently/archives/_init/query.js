function() {
  var ret = {}

  var descending = $(document).getUrlParam("descending")
  var startkey = $(document).getUrlParam("startkey")
  var endkey = $(document).getUrlParam("endkey")

  var pathname = document.location.pathname
  var viewname = pathname.substring(pathname.lastIndexOf("/") + 1)
  if (viewname == "recent-posts-by-topic" && startkey)
    ret.view = "archives-by-topic"
  else
    ret.view = "archives"

  if (descending)
    ret.descending = descending

  if (ret.view == "archives-by-topic") {
    if (startkey)
      ret.startkey = [JSON.parse(unescape(startkey))[0], descending == "true" ? "9999-12-31T23:59:59.999Z" : "0000-01-01T00:00:00.000Z"]
    if (endkey)
      ret.endkey = [JSON.parse(unescape(endkey))[0], descending == "true" ? "0000-01-01T00:00:00.000Z" : "9999-12-31T23:59:59.999Z"]
  }

  return ret
}
