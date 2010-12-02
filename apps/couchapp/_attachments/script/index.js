$.couch.app(function(app) {
  $("#account").evently(app.ddoc.evently.account, app)

  $("#posts").evently(app.ddoc.evently.posts, app)

  $(".date").prettyDate()

  // add subscriptions

  var path = app.require('vendor/couchapp/lib/path').init(app.req)  
  var uri = path.asset() + "/subscriptions.json"
  var ul = $("#subscriptions")

  $.get(uri, function(data) {
    var topics = JSON.parse(data).topics
    topics.forEach(function(topic) {
      var link = path.list('index', 'recent-posts-by-topic',
        {descending : true, limit: 10,
         startkey : [topic.url, "9999-12-31T23%3A59%3A59.999Z"],
         endkey : [topic.url, "0000-01-01T00%3A00%3A00.000Z"]})
      ul.append(
        '<li><a title="' + topic.description + '" href="' + link + '">' +
        topic.title + '</a></li>')
    })

    var as = $("#subscriptions li a")
    for (var i = 0; i < as.length; i++) {
      var startkey = $(document).getUrlParam("startkey")
      if (startkey && JSON.parse(unescape(startkey))[0] == JSON.parse(unescape($(as[i]).getUrlParam("startkey")))[0]) {
        as[i].style.fontWeight = "bold"
        break
      }
    }
  })

  $("#archives").evently(app.ddoc.evently.archives, app)
})
