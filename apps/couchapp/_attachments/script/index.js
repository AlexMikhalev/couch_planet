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


  var ceaseFire = false

  var scrollFun = function() {
    if (ceaseFire)
      return false

    var pathname = document.location.pathname
    var viewname = pathname.substring(pathname.lastIndexOf("/") + 1)

    var descending = $(document).getUrlParam("descending") || "true"

    var start = $(document).getUrlParam("startkey")
    var end_date = descending == "true" ?  "0000-01-01T00%3A00%3A00.000Z" : "9999-12-31T23%3A59%3A59.999Z"

    var date = $("#posts li.entry:last")[0].getAttribute("date")
    if (start && viewname == "recent-posts-by-topic") {
      var topic = JSON.parse(unescape(start))[0]
      var startkey = '["' + topic + '","' + date + '"]'
      var endkey = '["' + topic + '","' + end_date + '"]'
    } else {
      var startkey = '"' + date + '"'
      var endkey = '"' + end_date + '"'
    }

    var url = "../../_view/" + viewname + "?descending=" + descending +
              "&limit=5&skip=1&startkey=" + startkey + "&endkey=" + endkey
    var xhr = $.ajax({
      url : url,
      async: false
    })

    var data = null
    if ((xhr.status == 200 || xhr.status == 304))
      data = JSON.parse(xhr.responseText)

    if (data && data.rows) {
      if (data.offset == data.total_rows || data.rows.length < 4)
        ceaseFire = true
      var entries = data.rows
      var entriesXML = ""
      entries.forEach(function(entryObj) {
        var entry = entryObj.value
        entriesXML += '<li id="' + entry.link + '" date="' + entry.date +
                      '" class="entry"><h3><a href="' + entry.providerUrl +
                      '">' + entry.providerName + '</a></h3><h4><a href="' +
                      entry.link + '">' + entry.title + '</a></h4>' +
                      '<div class ="body">' + entry.body +
                      '<p class="by">by ' + entry.author +
                      ', <span class="date">' + $.prettyDate(entry.date) +
                      '</span></p></div></li>'
      })
      return entriesXML
    } else {
      return false
    }
  }

  $(document).endlessScroll({
    bottomPixels: 400,
    fireOnce: false,
    fireDelay: 500,
    data: scrollFun,
    loader: '<img id="loader" src="../../images/loading.gif"/>',
    insertAfter: "#posts li.entry:last",
    ceaseFire: function() {return ceaseFire}
  })
})
