function(e, r) {
  return {
    main_page : !subscriptions.init ? true : false,
    name : r.userCtx.name,
    uri_name : encodeURIComponent(r.userCtx.name),
    auth_db : encodeURIComponent(r.info.authentication_db)
  }
}
