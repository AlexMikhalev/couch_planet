function() {
  var elem = $(this)
  $.couch.logout({
    success : function() {
      elem.trigger("_init")
      if (subscriptions.init)
        document.location = "_list/index/recent-posts?descending=true&limit=10"
    }
  })
}
