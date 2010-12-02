function(resp) {
  var app = $$(this).app
  var path = app.require('vendor/couchapp/lib/path').init(app.req)
  var years = []
  var months = {}
  var posts = {}
  var archives = {'count': 0, 'year_archives': []}
  var month_map = {'01': 'January', '02': 'February', '03': 'March', '04': 'April', '05': 'May', '06': 'June', '07': 'July', '08': 'August', '09': 'September', '10': 'October', '11': 'November', '12': 'December'}
  var prev_year = null
  var prev_month = null

  resp.rows.forEach(function(r) {
    if (typeof(r.key) == "string")
      var date = r.key
    else
      var date = r.key[1]
    var year = date.substr(0, 4)
    var month = date.substr(5, 2)

    // the rows are sorted, you know
    if (year != prev_year) {
      years.push(year)
      months[year] = []
      posts[year] = {}
    }
    if (month != prev_month) {
      months[year].push(month)
      posts[year][month] = []
    }

    posts[year][month].push({
      'title' : r.value[0],
      'link' : r.value[1]
    })

    prev_year = year
    prev_month = month
  })

  $.each(years, function(i1, y) {
    var per_year_count = 0

    var pathname = document.location.pathname
    var viewname = pathname.substring(pathname.lastIndexOf("/") + 1)

    var descending = $(document).getUrlParam("descending") == "false" ? false : true

    var startkey = y + '-12-31T23:59:59.999Z'
    var endkey = y + '-01-01T00:00:00.000Z'
    if (viewname == 'recent-posts-by-topic') {
      var start = $(document).getUrlParam("startkey")
      topic = start ? JSON.parse(unescape(start))[0] : descending ? "z" : "0"
      startkey = [topic, startkey]
      endkey = [topic, endkey]
    }
    var year_link = path.list('index', viewname, {descending : descending, startkey : descending ? startkey : endkey, endkey : descending ? endkey : startkey})

    archives['year_archives'].push({'year': y, 'year_link': year_link, 'month_archives': []})

    $.each(months[y], function(i2, m) {
      var per_month_count = posts[y][m].length
      per_year_count += per_month_count

      var startkey = y + '-' + m + '-31T23:59:59.999Z'
      var endkey = y + '-' + m + '-01T00:00:00.000Z'
      if (viewname == 'recent-posts-by-topic') {
        var start = $(document).getUrlParam("startkey")
        topic = start ? JSON.parse(unescape(start))[0] : descending ? "0" : "z"
        var topic = JSON.parse(unescape($(document).getUrlParam("startkey")))[0]
        startkey = [topic, startkey]
        endkey = [topic, endkey]
      }
      var month_link = path.list('index', viewname, {descending : descending, startkey : descending ? startkey : endkey, endkey : descending ? endkey : startkey})

      archives['year_archives'][i1]['month_archives'].push({'month_name': month_map[m], 'month_link': month_link, 'count': per_month_count, 'posts': []})

      $.each(posts[y][m], function(i3, p) {
        archives['year_archives'][i1]['month_archives'][i2]['posts'].push(p)
      })
    })

    archives['year_archives'][i1]['count'] = per_year_count
  })

  return archives
}
