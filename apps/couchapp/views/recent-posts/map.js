function(doc) {
  if (doc.type == "Activity Stream" && doc.verb == "post") {
    var ret = {}
    var date = new Date(doc.postedTime)
    ret.date = date
    ret.title = doc.title
    ret.link = doc.object.id
    ret.body = doc.object.summary
    ret.providerName = doc.provider.name
    ret.providerUrl = doc.provider.id
    if (!doc.actor.name)
      ret.author = ret.providerName
    else
      ret.author = doc.actor.name
    if (!doc.actor.link)
      ret.authorUrl = ret.providerUrl
    else
      ret.authorUrl = doc.actor.link
    emit(date, ret)
  }
}
