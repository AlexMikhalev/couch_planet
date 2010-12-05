function(head, req) {
  var ddoc = this;
  var Mustache = require("lib/mustache");
  var List = require("vendor/couchapp/lib/list");
  var path = require("vendor/couchapp/lib/path").init(req);
  var Atom = require("vendor/couchapp/lib/atom");

  var indexPath = path.list('index','recent-posts',{descending:true, limit:10});
  var feedPath = path.list('index','recent-posts',{descending:true, limit:10, format:"atom"});

  // The provides function serves the format the client requests.
  // The first matching format is sent, so reordering functions changes 
  // thier priority. In this case HTML is the preferred format, so it comes first.
  provides("html", function() {
    var key = ""
    // render the html head using a template
    var stash = {
      header : {
        index : indexPath,
        blogName : ddoc.blog.title
      },
      blogName : ddoc.blog.title,
      sidebar : {feedPath : feedPath},
      scripts : {},
      db : req.path[0],
      design : req.path[2],
      feedPath : feedPath,
      newPostPath : path.show("edit"),
      assets : path.asset(),
      posts : List.withRows(function(row) {
        var post = row.value
        key = row.key
        return {
          title : post.title,
          author : post.author,
          providerName : post.providerName,
          providerUrl : post.providerUrl,
          date : post.date,
          body : post.body,
          link : post.link,
        };
      })
    };
    return Mustache.to_html(ddoc.templates.index, stash, ddoc.templates.partials, List.send);
  });

  // if the client requests an atom feed and not html, 
  // we run this function to generate the feed.
  provides("atom", function() {    
    var path = require("vendor/couchapp/lib/path").init(req);
    var markdown = require("vendor/couchapp/lib/markdown");
    var textile = require("vendor/textile/textile");

    // we load the first row to find the most recent change date
    var row = getRow();
    
    // generate the feed header
    var feedHeader = Atom.header({
      updated : (row ? new Date(row.value.date) : new Date()),
      title : ddoc.blog.title,
      feed_id : path.absolute(indexPath),
      feed_link : path.absolute(feedPath),
    });
    
    // send the header to the client
    send(feedHeader);

    // loop over all rows
    if (row) {
      do {
        if (row.value.format == "markdown") {
          var html = markdown.encode(row.value.body);
        } else if (row.value.format == "textile") {
          var html = textile.encode(row.value.body);
        } else {
          var html = row.value.body
        }
        // generate the entry for this row
        var feedEntry = Atom.entry({
          entry_id : path.absolute('/'+encodeURIComponent(req.info.db_name)+'/'+encodeURIComponent(row.id)),
          title : row.value.title,
          content : html,
          updated : new Date(row.value.date),
          author : row.value.author,
          alternate : path.absolute(path.show('post', row.id))
        });
        // send the entry to client
        send(feedEntry);
      } while (row = getRow());
    }

    // close the loop after all rows are rendered
    return "</feed>";
  });
};
