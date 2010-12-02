function() {
  var ret = {}

  var pathname = document.location.pathname
  var viewname = pathname.substring(pathname.lastIndexOf("/") + 1)
  if (viewname == "recent-posts-by-topic")
    ret.view = "recent-posts-by-topic"
  else
    ret.view = "recent-posts"

  var descending = $(document).getUrlParam("descending")
  var skip = $(document).getUrlParam("skip")
  var limit = $(document).getUrlParam("limit")
  var startkey = $(document).getUrlParam("startkey")
  var endkey = $(document).getUrlParam("endkey")

  if (descending)
    ret.descending = descending
  if (skip)
    ret.skip = skip
  if (limit)
    ret.limit = limit
  if (startkey)
    ret.startkey = JSON.parse(unescape(startkey))
  if (endkey)
    ret.endkey = JSON.parse(unescape(endkey))

  return ret
}
