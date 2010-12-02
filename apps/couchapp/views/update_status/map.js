function(doc) {
  if (doc.type == "Activity Stream" && doc.verb == "post")
    emit(doc.provider.id, [doc.object.id, doc.postedTime, doc._rev])
}
