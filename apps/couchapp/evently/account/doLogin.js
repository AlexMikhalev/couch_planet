function(e, name, pass) {
  var elem = $(this)
  $.couch.login({
    name : name,
    password : pass,
    success : function(r) {
      elem.trigger("_init")
      if (!subscriptions.init)
        document.location = "../../subscriptions.html"
    }
  })
}
