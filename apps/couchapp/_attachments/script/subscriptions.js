var subscriptions = {

  init : function() {
    var page = new this.Page()
    $("#toolbar button.add").click(page.addField)
    $("#toolbar button.save").click(page.save)

    var xhr = $.get("subscriptions.json", function(data, success, xhr) {
      page.rev = xhr.getResponseHeader("etag")
      var topics = JSON.parse(data).topics
      topics.forEach(function(topic) {
        page.addRow(topic.title, topic.description, topic.url)
      })
      page.isDirty = false
    })
  },

  Page : function() {
    var page = this

    this.rev = null
    this.isDirty = false

    this.save = function() {
      var json = '{\n  "topics": ['
      var topics = $("#subscriptions tr")
      for (var i = 0; i < topics.length; i++) {
        if (i != 0)
          json = json + ',\n'
        else
          json = json + '\n'
        var row1 = topics[i].firstElementChild
        var row2 = row1.nextSibling
        var row3 = row2.nextSibling
        var row1Value = '"' + row1.textContent + '"' || '""'
        var row2Value = '"' + row2.textContent + '"' || '""'
        var row3Value = '"' + row3.textContent + '"' || '""'
        json = json +
               '    {"title": ' + row1Value +
               ', "description": ' + row2Value +
               ', "url": ' + row3Value + '}'
      }
      json = json + '\n  ]\n}'
      $.ajax({
        type : "PUT",
        url : "subscriptions.json",
        data : json,
        dataType : "application/json",
        beforeSend : function(xhr) {
          xhr.setRequestHeader("If-Match", page.rev)
        },
        success : function() {
          page.isDirty = false
          document.location = document.location
        },
        error : function(xhr) {
          alert(xhr.responseText)
        }
      })
    }

    this.addRow = function(col1, col2, col3) {
      var row = $("<tr><th></th><td></td><td></td></tr>")
        .find("th").append($("<b></b>").text(col1)).end()
        .appendTo("#fields tbody.content")
        _initKey(row, col1)
        _initValue(row, col1)
      $("#fields tbody.content tr").removeClass("odd").filter(":odd").addClass("odd")
      row.find("td").first().text(col2)
      row.find("td").last().text(col3)
    }

    this.addField = function() {
      var fieldName = "unnamed"
      var row = _addRowForField(fieldName)
      page.isDirty = true
      row.find("th b").dblclick()
    }

    function _addRowForField(fieldName) {
      var row = $("<tr><th></th><td></td><td></td></tr>")
        .find("th").append($("<b></b>").text(fieldName)).end()
        .appendTo("#fields tbody.content")
        _initKey(row, fieldName)
        _initValue(row, fieldName)
      $("#fields tbody.content tr").removeClass("odd").filter(":odd").addClass("odd")
      return row
    }

    function _initKey(row, fieldName) {
      var cell = row.find("th")
      $("<button type='button' class='delete' title='Delete field'></button>").click(function() {
        row.remove()
        page.isDirty = true
        $("#fields tbody.content tr").removeClass("odd").filter(":odd").addClass("odd")
      }).prependTo(cell)
      cell.find("b").makeEditable({
        allowEmpty: false,
        accept: function(newName, oldName) {
          $(this).text(newName)
          page.isDirty = true
        },
        begin: function() {
          row.find("th button.delete").hide()
          return true
        },
        end: function(keyCode) {
          row.find("th button.delete").show()
          if (keyCode == 9) { // tab, move to editing the next column
            row.find("td").first().dblclick()
          }
        },
      })
    }

    function _initValue(row, fieldName) {
      row.find("td").makeEditable({acceptOnBlur: false, allowEmpty: true,
        createInput: function(value) {
          var elem = $(this)
          if (elem.find("dl").length > 0 ||
              elem.find("code").is(".array, .object") ||
              typeof(value) == "string" && (value.length > 60 || value.match(/\n/))) {
            return $("<textarea rows='1' cols='40' spellcheck='false'></textarea>")
          }
          return $("<input type='text' spellcheck='false'>")
        },
        end: function(keyCode) {
          if (keyCode == 9) // tab, move to editing the next column
            if (this.cellIndex == 1)
              row.find("td").last().dblclick()
            else
              row.find("th b").first().dblclick()
        },
        prepareInput: function(input) {
          if ($(input).is("textarea")) {
            var height = Math.min(input.scrollHeight, document.body.clientHeight - 100)
            $(input).height(height).makeResizable({vertical: true}).enableTabInsertion()
          }
        },
        accept: function(newValue) {
          $(this).text(newValue)
          page.isDirty = true
        },
      })
    }

    window.onbeforeunload = function() {
      if (page.isDirty)
        return "You've made changes to this document that have not been " +
               "saved yet."
    }
  }
}
