$.log = function(m) {
  if (window && window.console && window.console.log) {
    window.console.log(arguments.length == 1 ? m : arguments);
  }
};

// http://stackoverflow.com/questions/1184624/serialize-form-to-json-with-jquery/1186309#1186309
$.fn.serializeObject = function() {
    var o = {};
    var a = this.serializeArray();
    $.each(a, function() {
        if (o[this.name]) {
            if (!o[this.name].push) {
                o[this.name] = [o[this.name]];
            }
            o[this.name].push(this.value || '');
        } else {
            o[this.name] = this.value || '';
        }
    });
    return o;
};

// todo remove this crap
function escapeHTML(st) {                                       
  return(                                                                 
    st && st.replace(/&/g,'&amp;').                                         
      replace(/>/g,'&gt;').                                           
      replace(/</g,'&lt;').                                           
      replace(/"/g,'&quot;')                                         
  );                                                                     
};

function safeHTML(st, len) {
  return st ? escapeHTML(st.substring(0,len)) : '';
}

// todo this should take a replacement template
$.linkify = function(body) {
  return body.replace(/((ftp|http|https):\/\/(\w+:{0,1}\w*@)?(\S+)(:[0-9]+)?(\/|\/([\w#!:.?+=&%@!\-\/]))?)/gi,function(a) {
    return '<a target="_blank" href="'+a+'">'+a+'</a>';
  }).replace(/\@([\w\-]+)/g,function(user,name) {
    return '<a href="#/mentions/'+encodeURIComponent(name.toLowerCase())+'">'+user+'</a>';
  }).replace(/\#([\w\-\.]+)/g,function(word,tag) {
    return '<a href="#/tags/'+encodeURIComponent(tag.toLowerCase())+'">'+word+'</a>';
  });
};

$.fn.prettyDate = function() {
  $(this).each(function() {
    $(this).text($.prettyDate($(this).text()));    
  });
};

$.prettyDate = function(time){
  
	var date = new Date(time.replace(/-/g,"/").replace("T", " ").replace("Z", " +0000").replace(/(\d*\:\d*:\d*)\.\d*/g,"$1")),
		diff = (((new Date()).getTime() - date.getTime()) / 1000),
		day_diff = Math.floor(diff / 86400);

  if (isNaN(day_diff)) return time;

	return day_diff < 1 && (
			diff < 60 && "just now" ||
			diff < 120 && "1 minute ago" ||
			diff < 3600 && Math.floor( diff / 60 ) + " minutes ago" ||
			diff < 7200 && "1 hour ago" ||
			diff < 86400 && Math.floor( diff / 3600 ) + " hours ago") ||
		day_diff == 1 && "yesterday" ||
		day_diff < 21 && day_diff + " days ago" ||
		day_diff < 45 && Math.ceil( day_diff / 7 ) + " weeks ago" ||
		day_diff < 730 && Math.ceil( day_diff / 31 ) + " months ago" ||
		Math.ceil( day_diff / 365 ) + " years ago";
};

$.argsToArray = function(args) {
  if (!args.callee) return args;
  var array = [];
  for (var i=0; i < args.length; i++) {
    array.push(args[i]);
  };
  return array;
}

jQuery.fn.extend({
/**
* Returns HTTP GET parameters.
*
* Returns null if the desired parameter does not exist.
*
* To get a document parameter:
* @example value = $(document).getUrlParam("paramName")
* 
* To get a parameters of an html element (uses src or href attribute):
* @example value = $('#imgLink').getUrlParam("paramName")
*/ 
  getUrlParam: function(strParamName) {
	  strParamName = escape(unescape(strParamName))
	  
	  var returnVal = new Array()
	  var qString = null
	  
	  if ($(this).attr("nodeName") == "#document") {
	  	//document-handler
		
		  if (window.location.search.search(strParamName) > -1 )
			  qString = window.location.search.substr(1,window.location.search.length).split("&")
			
	  } else if ($(this).attr("src")) {
	  	
	  	var strHref = $(this).attr("src")
	  	if (strHref && strHref.indexOf("?") > -1) {
	    	var strQueryString = strHref.substr(strHref.indexOf("?")+1)
	  		qString = strQueryString.split("&")
	  	}
	  } else if ($(this).attr("href")) {
	  	
	  	var strHref = $(this).attr("href")
	  	if (strHref && strHref.indexOf("?") > -1) {
	    	var strQueryString = strHref.substr(strHref.indexOf("?") + 1)
	  		qString = strQueryString.split("&")
	  	}
	  } else {
	  	return null
	  }
	  	
	  if (qString == null )
      return null
	  
	  for (var i=0; i < qString.length; i++) {
			if (escape(unescape(qString[i].split("=")[0])) == strParamName)
				returnVal.push(qString[i].split("=")[1])
	  }
	  
	  if (returnVal.length == 0)
      return null
	  else if (returnVal.length == 1)
      return returnVal[0]
	  else
      return returnVal
	}
})
