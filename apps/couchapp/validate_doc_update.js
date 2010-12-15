function (newDoc, oldDoc, userCtx, secObj) {
  if (userCtx.roles.indexOf("_admin") == -1)
    throw({unauthorized : "Only admin users may update the database."})
}
