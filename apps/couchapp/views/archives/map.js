function(doc) {
  if (doc.type == "Activity Stream" && doc.verb == "post")
    emit(new Date(doc.postedTime), [doc.title, doc.object.id])
}
