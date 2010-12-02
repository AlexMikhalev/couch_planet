function(doc) {
  if (doc.type == "Activity Stream" && doc.verb == "post") {
    emit([doc.provider.id, new Date(doc.postedTime)], [doc.title, doc.object.id])
  }
}
