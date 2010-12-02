function (newDoc, oldDoc, userCtx, secObj) {
  var v = require("lib/validate").init(newDoc, oldDoc, userCtx, secObj)

  if (!v.isAdmin()) {    
    v.unauthorized("Only admin users may update the database.")
  }
}
