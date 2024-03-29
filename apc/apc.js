/**
 * @file Contains front-end code for the Age-Period-Cohort Analysis Tool
 */

/**
 * JQuery plugins implementing data-check-show="#target" and data-check-hide="#target"
 */

$(function() {
  $('[data-check-show]').change(function() {
    var target = $(this).attr('data-check-show');
    this.checked
      ? $(target).show()
      : $(target).hide()
  })

  $('[data-check-hide]').change(function() {
    var target = $(this).attr('data-check-hide');
    this.checked
      ? $(target).hide()
      : $(target).show()
  })
})

/**
 * JQuery plugins implementing data-check-enabled="#target" and data-check-disabled="#target"
 */

$(function() {
  $('[data-check-enabled]').change(function() {
    var target = $(this).attr('data-check-enabled');
    this.checked
      ? $(target).prop('disabled', false)
      : $(target).prop('disabled', true)
  })

  $('[data-check-disabled]').change(function() {
    var target = $(this).attr('data-check-disabled');
    this.checked
      ? $(target).prop('disabled', true)
      : $(target).prop('disabled', false) 
  })
})

$(document).ready(function () {
  // attach event handlers
  APC.setInputs({
    title: $('#title'),
    description: $('#description'),
    startYear: $('#startYear'),
    startAge: $('#startAge'),
    interval: $('#interval'),
    file: $('#file'),
    defaultReference: $('#defaultReference'),
    manualReference: $('#manualReference'),
    referenceAge: $('#referenceAge'),
    referencePeriod: $('#referencePeriod'),
    referenceCohort: $('#referenceCohort'),
  });

  // show unsupported browser modal if needed
  if (window.navigator.userAgent.match(/Trident|MSIE/)) {
    $('#unsupported-browser-modal').modal('show');
  }

  // set form handlers
  $('#clear').click(APC.clear)
  $('#calculate').click(APC.calculate)

  // set handler for downloading files
  $('#download').click(function() {
    var link = $('#download-format').val()

    if (link == 'Excel output')
      Excel.createDownload(APC.getExcelData())
    else
      window.open(link, 'download')
  })

  // allow user to paste table data
  $('#paste-area').bind('paste', function (e) {
    var data = e.originalEvent.clipboardData.getData('text')
    var table = data.match(/[^\r\n]+/g).map(function (line) {
      var values = line.split(/\s/).map(parseFloat)
      // return null for any non-numeric inputs and input lengths not divisible by 2
      return (values.includes(NaN) || values.length % 2) ? null : values
    })

    if (!table.includes(null) && table.length > 1)
      APC.updateTable(table)
      $('#upload').prop('checked', false)
      $('#paste').prop('checked', true)
  })
})


/**
 * @namespace FileInput
 * @description Handles parsing of input files as data models
 * Exports the following functions to handle file input:
 * FileInput.parse(file : File) : Promise <object, string>
 */
var FileInput = (function () {
  return {
    parse: parseFile,
    parseFileContents: parseFileContents,
  }

  /**
   * @function parseFile
   * @summary Generates an input object from a file
   * @param {File} file - An input file
   * @returns {PromiseLike<Object>} Returns a promise that resolves to an input object
   * @description An input object has the following properties:
   * {
   *  title: {string} Description of data
   *  description: {string} Optional details
   *  startYear: {number} The first year of the first calendar period of the data
   *  startAge: {number} The first age of the first age group of the data
   *  interval: {number} The width of the age and period intervals
   *  table: {number[][]} Table containing count/population data
   * }
   *
   * @example
   * FileInput
   *   .parse(document.getElementById('file1').files[0])
   *   .then(console.log)
   */
  function parseFile (file) {
    return new Promise(
      function (resolve, reject) {
        if (window.FileReader && file && file instanceof File) {

          /** @type {FileReader} */
          var reader = new FileReader()

          reader.readAsText(file)
          reader.onerror = reject.bind(this, 'Failed to parse input file')

          reader.onload = function (event) {
            // select non-empty lines
            /** @type {string[]} */
            var text = event.currentTarget.result;
            resolve(parseFileContents(text))
          }
        }
      }
    )
  }

  function parseFileContents(contents) {
    contents = contents.match(/[^\r\n]+/g)
    return ({
      title: parseHeader(contents.shift()),
      description: parseHeader(contents.shift()),
      startYear: parseInt(parseHeader(contents.shift())),
      startAge: parseInt(parseHeader(contents.shift())),
      interval: parseInt(parseHeader(contents.shift())),
      table: contents.map(parseLine)
    })
}


  /**
   * @function parseLine
   * @summary Parses a line of a csv file as an array of floats
   * @param {string} line - A line containing comma-separated values
   * @returns {number[]} An array containing parsed numeric values
   */
  function parseLine (line) {
    return line.split(',').map(parseFloat)
  }

  /**
   * @function parseHeader
   * @summary Extracts a header from a line (title, description, etc)
   * @param {string} line - A line containing header data
   * @returns {string} The portion of the line after the first colon
   */
  function parseHeader (line) {
    /** @type {string[]}  */
    var description = line.match(/"(.*?)"/)

    /** @type {string} */
    var header = description ? description[1] : line.split(',')[0]

    return header.substring(header.indexOf(':') + 1).trim()
  }
})()

/**
 * @namespace DataTable
 * @description Creates table nodes generated by DataTables
 * Exports the following functions:
 * DataTable.createEmpty(numRows: number, numCols: number) : HTMLTableElement
 * DataTable.createInput(input: object) : HTMLTableElement
 * DataTable.createOutput(output: object) : HTMLTableElement
 */
var DataTable = (function () {
  return {
    createEmpty: createEmptyTable,
    createInput: createInputTable,
    createOutput: createOutputTable
  }

  /**
   * @function createEmptyTable
   * @summary Creates an empty table of the specified size
   * @param {number} numRows - The number of rows
   * @param {number} numCols - The number of columns
   * @returns {HTMLTableElement} An empty table
   */
  function createEmptyTable (numRows, numCols) {
    return createTable(
      createColumns(0, numCols),
      createMatrix(numRows, numCols + 1, '&zwj;'),
      'input-table table-background-instructions table table-striped table-bordered table-sm border-0')
  }

  /**
   * @function createInputTable
   * @summary Creates a table for displaying input data
   * @description An input object has the following properties:
   * {
   *  title: string,
   *  description: string,
   *  startYear: number,
   *  startYear: number,
   *  interval: number,
   *  table: number[][]
   * }
   *
   * @param {Object} input - The input object to display
   * @returns {HTMLTableElement} A table containing input data
   */
  function createInputTable (input) {
    // create a table with count/population columns
    var table = createTable(
      createColumns(input.table.length, input.table[0].length),
      getData(input),
      'input-table table table-striped table-bordered table-sm border-0')

    // insert additional table headers
    createHeaders(input).forEach(function (header) {
      table.tHead.insertAdjacentHTML('afterbegin', header.outerHTML)
    })

    return table
  }

  /**
   * @function createOutputTable
   * @summary Creates a table for displaying output data
   * @param {string|number[]} output - The data to display
   * @param {string[]} headers - The headers for this table
   * @param {string} [headers] - A title for this table
   * @returns {HTMLTableElement} An HTMLTableElement containing output data
   */
  function createOutputTable (output, headers, title) {
    var table = createTable(
      getColumns(headers),
      round(output, title == 'Wald Tests' ? 4 : 3),
      'output-table table table-striped table-bordered table-sm')

    if (title)
      table.tHead.insertAdjacentHTML('afterbegin', createTitle(output, title).outerHTML)

    return table
  }

  /**
   * @function createTable
   * @summary Creates a DataTable
   * @description
   * Column data is an array of objects containing properties for each column:
   * [
   *  {
   *    data: {string=} Corresponding object key, needed only when using objects
   *    title: {string} The display title for this column
   *    className: {string} The css class to apply
   *  },
   *  ...
   * ]
   *
   *
   * Display data can be either an array of arrays, or an array objects
   *
   * Array of arrays:
   * [
   *  [valueA, valueB, valueC],
   *  [valueA, valueB, valueC],
   *  ...
   * ]
   *
   * Array of objects:
   * [
   *  {columnA: number, columnB: number},
   *  {columnA: number, columnB: number},
   *  ...
   * ]
   *
   * @param {{data: string, title: string, className: string}[]} columns - The column names of the data
   * @param {Object[]|number[][]} data - The data to display
   * @param {string} classname - The css classes to apply to this table
   */
  function createTable (columns, data, classname) {

    /** @type {HTMLTableElement} */
    var table = document.createElement('table')
    table.className = classname
    table.width = '100%'

    $(table).DataTable({
      destroy: true,
      columns: columns,
      data: data,
      bSort: classname.includes('output'),
      bFilter: false,
      paging: false,
      responsive: true,
      aaSorting: [],
      dom: 't'
    })

    return table
  }

  /**
   * @function round
   * @summary Rounds a table to a specified place
   * @param {number[][]} table - The input matrix
   * @param {number} digits - The number of places to round to
   * @returns {number[][]} A numeric matrix containing rounded values
   */
  function round(table, digits) {
    return table.map(function(row) {
      return row.map(function(value) {
        return +value ? +parseFloat(value).toFixed(digits) : value
      })
    })
  }

  /**
   * @function createTitle
   * @summary Creates a title row for table
   * @param {number|string[][]} output - The output matrix
   * @param {string} title - The title of the matrix
   * @returns {HTMLTableRowElement} A row containing the title
   */
  function createTitle(output, title) {
    /** @type HTMLTableRowElement */
    var titleRow = document.createElement('tr')

    /** @type HTMLTableHeaderElement */
    var titleHeader = document.createElement('th')

    titleHeader.colSpan = output[0].length
    titleHeader.innerHTML = title
    titleRow.appendChild(titleHeader)
    return titleRow
  }

  /**
   * @function createHeaders
   * @summary Creates table headers for input data
   * @description
   * An input model has the following properties:
   * {
   *  title: {string} Description of data
   *  description: {string} Optional details
   *  startYear: {number} The first year of the first calendar period of the data
   *  startAge: {number} The first age of the first age group of the data
   *  interval: {number} The width of the age and period intervals
   *  table: {number[][]} Table containing count/population data
   * }
   *
   * @param {Object} model - The input object to create table headers for
   * @returns {HTMLTableRowElement[]} Table rows containing header information
   */
  function createHeaders (model) {

    /** @type HTMLRowElement[] */
    var headers = []

    // create table row for title header
    /** @type HTMLTableRowElement */
    var titleRow = document.createElement('tr')

    // create spacer for both headers
    /** @type HTMLTableHeaderElement */
    var spacer = document.createElement('th')
    spacer.style.visibility = 'hidden'
    titleRow.appendChild(spacer)

    // create header for title/description
    /** @type HTMLTableHeaderElement */
    var titleHeader = document.createElement('th')
    titleHeader.className = 'bg-light-gray'
    titleHeader.colSpan = model.table[0].length
    titleHeader.innerHTML = (model.title || 'Created ' + new Date().toLocaleString()) +
      '<div class="text-primary">' + (model.description || 'No description') + '</div>'
    titleRow.appendChild(titleHeader)
    headers.push(titleRow)

    // create row for year ranges
    /** @type HTMLTableRowElement */
    var yearRow = document.createElement('tr')

    /** @type number */
    var endYear = model.startYear + model.interval * model.table[0].length / 2

    // add each header to the row
    for (var year = model.startYear; year < endYear; year += model.interval) {
      /** @type HTMLTableHeaderElement */
      var yearHeader = document.createElement('th')
      yearHeader.className = 'bg-light'
      yearHeader.colSpan = 2

      // if year interval is one, display single years instead of a year range
      /** @type string */
      var yearRange = model.interval > 1
        ? [year, year + model.interval - 1].join('-')
        : year

      yearHeader.innerText = yearRange
      yearRow.appendChild(yearHeader)
    }

    if (model.startYear && model.interval && model.table)
      headers.unshift(yearRow)

    spacer.rowSpan = headers.length
    return headers
  }

  /**
   * @function createColumns
   * @summary Creates alternating count/population columns for the input table
   * @description
   * Creates column names in the following format:
   * [
   *  {
   *    title: {string} 'Age' | 'N Age Groups'
   *    className: {string} 'grey'
   *  },
   *  {
   *    title: {string} 'Count'
   *    className: {string} 'dt-body-right'
   *  },
   *  {
   *    title: {string} 'Population'
   *    className: {string} 'dt-body-right'
   *  },
   *  ...
   * ]
   *
   * @param {number} numRows The number of rows in this table
   * @param {number} numCols The number of columns in this table
   * @returns {Object[]} Column names used by DataTables
   */
  function createColumns (numRows, numCols) {
    /** @type Object[] */
    var columns = [{
      title: numRows
        ? '<small class="d-block px-0 font-weight-bold text-center">' + numRows + ' age groups</small>'
        : 'Age',
      className: numRows ? 'bg-light-gray table-body-text-right' : 'bg-light-gray'
    }]

    while (numCols--)
      columns.push({
        title: numCols % 2 ? 'Count' : 'Population',
        className: 'table-body-text-right'
      })

    return columns
  }

  /**
   * @function createMatrix
   * @summary Creates a matrix with a specified initial size and fill value
   * @param {number} numRows - The number of rows in this matrix
   * @param {number} numCols - The number of columns in this matrix
   * @param {string | number} initialValue - The fill value for this matrix
   * @returns {(string|number)[][]} A matrix of the specified size
   */
  function createMatrix (numRows, numCols, initialValue) {
    return Array(numRows)
      .fill(Array(numCols)
        .fill(initialValue || null))
  }

  /**
   * @function getData
   * @summary Prepends age ranges to each row in the input table
   * @description
   * An input model contains the following properties:
   * {
   *  title: {string} Description of data
   *  description: {string} Optional details
   *  startYear: {number} The first year of the first calendar period of the data
   *  startAge: {number} The first age of the first age group of the data
   *  interval: {number} The width of the age and period intervals
   *  table: {number[][]} Table containing count/population data
   * }
   *
   * @param {Object} model
   * @returns {(string|number)[][]} A matrix containing age, count, and population data
   */
  function getData (model) {
    return model.table.map(function (row, index) {
      // calculates the age from the starting age, index and interval
      /** @type number|string */
      var age = model.startAge + index * model.interval || ''

      if (parseInt(age) && model.interval > 1)
        age += '-' + (age + model.interval - 1)

      return [age].concat(row)
    })
  }

  /**
   * @function getColumns
   * @summary Creates column headers for the output table
   * @description
   *
   * Column data is an array of objects containing properties for each column:
   * [
   *  {
   *    data: {string=} Corresponding object key, needed only when using objects
   *    title: {string} The display title for this column
   *    className: {string} The css class to apply
   *  },
   *  ...
   * ]
   *
   * @param {string[]} output
   * @returns
   */
  function getColumns (headers) {
    return headers.map(function (header) {
      return {
        title: header,
        className: 'table-body-text-right'
      }
    })
  }
})()


/**
 * @namespace Excel
 * @description Exports results data as an excel document
 * Exports the following functions:
 * Excel.exportData(data : Object[])
 */
var Excel = (function () {
  return {
    createDownload: createDownload
  }

  /**
   * @function createDownload
   * @summary Exports output to an excel file and downloads it
   * @param {Object[]} output - Data for each sheet
   */
  function createDownload (output) {
    var workbook = new ExcelBuilder.Workbook()

    output.forEach(function (data) {
      workbook.addWorksheet(generateSheet(workbook, data))
    })

    ExcelBuilder.Builder.createFile(workbook).then(
      download.bind(null, 'APC_Analysis_' + getTimestamp() + '.xlsx',
      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
    )
  }

  /**
   * @function generateSheet
   * @summary Creates an excel sheet to be added to a workbook
   * @description
   * Sheet data is an object with the following properties:
   * {
   *  title {string} The sheet title
   *  table {string|number[][]} The output table
   *  image {string} A base64 png graph
   * }
   *
   * @param {ExcelBuilder.Workbook} workbook - The workbook to operate on
   * @param {Object} data - Sheet data
   * @returns {ExcelBuilder.Worksheet} A sheet containing exported data
   */
  function generateSheet (workbook, data) {
    var worksheet = workbook.createWorksheet({name: data.title})

    // add table
    worksheet.setData(data.table)

    // add graph
    if (data.image) {
      var drawings = new ExcelBuilder.Drawings()

      var graphref = workbook.addMedia(
        'image',
        data.title + '.png',
        data.image.replace('data:image/png;base64,', ''))

      var graphpic = new ExcelBuilder.Drawing.Picture()
      graphpic.createAnchor('oneCellAnchor', {
        x: data.table[0].length + 1,
        y: 0,
        width: ExcelBuilder.Positioning.pixelsToEMUs(600),
        height: ExcelBuilder.Positioning.pixelsToEMUs(500)
      })

      graphpic.setMedia(graphref)
      drawings.addDrawing(graphpic)
      worksheet.addDrawings(drawings)
      workbook.addDrawings(drawings)
    }

    return worksheet
  }

  /**
   * @function download
   * @summary Creates a file from a base64 string and downloads it
   * @param {string} filename - The file name
   * @param {string} type - The mimetype
   * @param {string} data - The data (as base64)
   */
  function download (filename, type, data) {
    var bytechars = atob(data)
    var bytenums = new Array(bytechars.length)

    for (var i = 0; i < bytechars.length; i++)
      bytenums[i] = bytechars.charCodeAt(i)

    var bytearr = new Uint8Array(bytenums)
    var blob = new Blob([bytearr], {type: type})

    if (window.navigator.msSaveOrOpenBlob)
      window.navigator.msSaveBlob(blob, filename)
    else {
      var element = window.document.createElement('a')

      if (typeof element.download == 'undefined')
        alert('Please open the downloaded file in Microsoft Excel')

      element.href = window.URL.createObjectURL(blob)
      element.download = filename
      document.body.appendChild(element)
      element.click()
      document.body.removeChild(element)
    }
  }

  /**
   * @function getTimestamp()
   * @summary creates a timestamp
   * @returns {string} A timestamp containing hours, minutes, and seconds
   */
  function getTimestamp () {
    var date = new Date()
    return [date.getHours(), date.getMinutes(), date.getSeconds()].join('_')
  }
})()


/**
 * @namespace APC
 * @description Creates input models, handles ajax calls,
 * and populates the DOM with results
 *
 * Exports the following functions:
 * - calculate
 * - clear
 * - setInputs
 * - updateTable
 * - getExcelData
 * - loadSample
 */
var APC = (function () {

  // Holds DOM references
  /** @type Object */
  var inputs = {
    title: null,
    description: null,
    startYear: null,
    startAge: null,
    interval: null,
    file: null,
    defaultReference: null,
    manualReference: null,
    referenceAge: null,
    referencePeriod: null,
    referenceCohort: null
  }

  /** @type Object */
  var data = {
    title: null,
    description: null,
    startYear: null,
    startAge: null,
    interval: null,
    table: null,
    reference: null
  }

  /** @type Object */
  var output = null

  return {
    calculate: calculate,
    clear: clear,
    setInputs: setInputs,
    updateTable: updateTable,
    getExcelData: getExcelData,
    loadSample: loadSample,
    downloadSamples: downloadSamples,
  }

    
  // load sample data (same sample data as found in help.html)
  function loadSample(path) {
    fetch(path)
    .then(function(response) { 
      return response.text() 
    })
    .then(function(text) { 
      updateUI(FileInput.parseFileContents(text));
      $('#upload').prop('checked', false)
      $('#paste').prop('checked', true)
    });
  }

  // download all sample csv files
  function downloadSamples() {
    for (var i = 0; i < arguments.length; i++) {
      var iframe = $('<iframe style="visibility: collapse;"></iframe>');
      $('body').append(iframe);
      var content = iframe[0].contentDocument;
      var form = '<form action="' + arguments[i] + '" method="GET"></form>';
      content.write(form);
      $('form', content).submit();
      setTimeout((function(iframe) {
        return function() { 
          iframe.remove(); 
        }
      })(iframe), 2000);
    }
  }   

  /**
   * @function setInputs
   * @summary Saves DOM references in the configuration and attaches event handlers
   * @description
   * A configuration object has the following properties:
   * {
   *  title {jQuery.HTMLInputElement} - A text input for the dataset's title
   *  description {jQuery.HTMLInputElement} - A text input for the description
   *  startYear {jQuery.HTMLInputElement} - A numeric input for the starting year
   *  startAge {jQuery.HTMLInputElement} - A numeric input for the starting age
   *  interval {jQuery.HTMLSelectElement} - A selector for the interval length
   *  file {jQuery.HTMLInputElement} - A file input element for the dataset
   *  defaultReference {jQuery.HTMLInputElement} - A radio button for selecting default reference values
   *  manualReference {jQuery.HTMLInputElement} - A radio button to select manual reference values
   *  referenceAge {jQuery.HTMLSelectElement} - A select element to select the reference age
   *  referencePeriod {jQuery.HTMLSelectElement} - A select element to select the reference period
   *  referenceAge {jQuery.HTMLInputElement} - A read-only text field to display the reference cohort
   * }
   *
   * @param {Object} config
   */
  function setInputs (config) {
    // set default values
    inputs = config
    output = {}
    data = {
      title: null,
      description: null,
      startYear: null,
      startAge: null,
      interval: null,
      table: null,
      reference: null
    }

    // attach event handlers
    for (element in inputs) {
      if ([ 'referenceAge',
            'referencePeriod',
            'referenceCohort'].includes(element))
        inputs[element].change(updateReferenceCohort)

      else
        inputs[element].change(update)
    }

    // initialize ui
    $('#errors').hide()
    $('#download-results').hide()
    $('[data-table]').find('table').DataTable().destroy()
    $('[data-table]').empty()
    updateUI()
  }


  /**
   * @function update
   * @summary Updates the UI
   * @this The input element that fired this event
   */
  function update () {
    // Updates the UI based on the contents of the file
    if (this.type === 'file')
      FileInput.parse(this.files[0]).then(updateUI)

    else
      for (key in inputs) {
        var input = inputs[key];
        var value = input.attr('type') == 'radio'
          ? input.prop('checked')
          : input.val()
        value = DOMPurify.sanitize(value);
        data[key] = isNaN(+value) ? value : +value
      }

    updateUI()
  }

  /**
   * @function updateUI
   * @summary Updates UI based on file contents and form data
   * @description
   * A file model contains the following properties:
   * {
   *  title: {string} Description of data
   *  description: {string} Optional details
   *  startYear: {number} The first year of the first calendar period of the data
   *  startAge: {number} The first age of the first age group of the data
   *  interval: {number} The width of the age and period intervals
   *  table: {number[][]} Table containing count/population data
   * }
   *
   * @param {Object} [model] A file model to update the UI with
   */
  function updateUI (model) {

    // if a file model was supplied, update the corresponding form elements as well
    if (model)
      for (key in model) {
        data[key] = model[key]
        if (inputs[key])
          inputs[key].val(model[key])
      }

    if (data.table && +data.startAge && +data.startYear && +data.interval) {
      updateReference()
    }

    else {
      clearReference()
    }

    /** @type HTMLTableElement */
    var table = data.table ? DataTable.createInput(data) : DataTable.createEmpty(12, 6)

    // add role information
    $(table).find('th').attr('role', 'column')

    // allow user to paste table information
    $(table).mouseup(focusPasteArea)

    // replace the current table
    $('#table').find('table').DataTable().destroy()
    $('#table').append(table)
    $('a[href="#Input"]').tab('show')
  }

  /**
   * @function clear
   * @summary Resets the application to its original state
   */
  function clear () {
    for (key in inputs)
      inputs[key].val('')

    for (key in data)
      data[key] = null

    clearReference();
    inputs.defaultReference.prop('checked', true).change();

    setInputs(inputs)

    // Enables input selection after clearing fields
    $('#paste').prop('disabled', false) 
    $('#upload').prop('disabled', false)
    if ($('#upload').prop('checked')) {
      $('#file').prop('disabled', false)
    } 

    // Enables sample links
    $('.sampleLink').prop('disabled', false)

    // Remove tooltip
    $('#input-panel').removeAttr('title')
  }

  /**
   * @function updateTable
   * @summary Updates the data table
   * @param {number[][]} table - The pasted table
   */
  function updateTable(table) {
    data.table = table
    updateUI()
  }

  /**
   * @function focusPasteAarea
   * @summary Focuses the paste area
   */
  function focusPasteArea() {
    var scroll = $('#table').scrollLeft()
    $('#paste-area').val('');
	  $('#paste-area').focus().select();
    $('#table').scrollLeft(scroll);
  }

  /**
   * @function updateReference
   * @summary Updates reference values for age and period ranges
   */
  function updateReference() {
    /** @type number[][] */
    var ageRange = createRanges(data.startAge, data.startAge + data.interval * data.table.length, data.interval)

    /** @type number[][] */
    var periodRange = createRanges(data.startYear, data.startYear + data.interval * data.table[0].length / 2, data.interval)

    inputs.referenceAge.html(createReferenceOptions(ageRange))
    inputs.referenceAge.prepend($('<option value="" hidden>Age</option>'))
    inputs.referencePeriod.html(createReferenceOptions(periodRange))
    inputs.referencePeriod.prepend($('<option value="" hidden>Year</option>'))

    inputs.referenceAge.val('');
    inputs.referencePeriod.val('');
    updateReferenceCohort()
  }

  /**
   * @function clearReference
   * @summary Clears reference values for age and period ranges
   */
  function clearReference() {
    inputs.referenceAge.html($('<option value="" hidden>Age</option>'))
    inputs.referencePeriod.html($('<option value="" hidden>Year</option>'))
    inputs.referenceAge.val('');
    inputs.referencePeriod.val('');
    updateReferenceCohort()
  }

  /**
   * @function updateReferenceCohort
   * @summary Updates reference cohort with the selected reference values
   */
  function updateReferenceCohort() {
    var referencePeriod = +DOMPurify.sanitize(inputs.referencePeriod.val())
    var referenceAge = +DOMPurify.sanitize(inputs.referenceAge.val())

    if (referenceAge && referencePeriod)
      inputs.referenceCohort.val(referencePeriod - referenceAge)
    else
      inputs.referenceCohort.val('')
  }

  /**
   * @function createRanges
   * @summary Creates ranges between the starting and ending values
   * @param {number} start - The start of the range
   * @param {number} end - The end of the range
   * @param {number} interval - The interval betwen range values
   * @returns {number[][]} An array containing ranges
   */
  function createRanges(start, end, interval) {
    var ranges = []

    for (var i = start; i < end; i += interval)
      ranges.push(interval == 1 ? [i] : [i, i - 1 + interval])

    return ranges
  }

  /**
   * @function createReferenceOptions
   * @summary Creates html options for selecting reference values
   * @param {number[][]} ranges The array of range values to use
   * @returns {HTMLOptionElement[]} An array of option elements
   */
  function createReferenceOptions(ranges) {
    if (ranges[0].length == 1) {
      return ranges.map(function(range) {
        range = range.map(Number);
        return $('<option>')
          .text(range[0])
          .val(range[0])
      });
    } else {
      return ranges.map(function(range) {
        range = range.map(Number);
        return $('<option>')
          .text(range.join('-'))
          .val((1 + range[0] + range[1]) / 2)
      });
    }
  }


  /**
   * @function validate
   * @summary Validates inputs and displays any errors
   * @returns {Boolean} true if validated, false otherwise
   */
  function validate () {

    var valid = $('#apc-form')[0].checkValidity() && data.table
    var messages = []

    if (inputs.manualReference.prop('checked') && (!inputs.referencePeriod.val() || !inputs.referencePeriod.val())) {
      messages.push('When specifying references manually, both reference age and reference year should be provided')
      valid = false;
    }

    if (valid)
      $('#errors').hide()

    else {
      if ($('#paste').is(':checked')) {
        if (!data.table)
        messages.push('Input table is required')

        for (key in inputs) {
          var title = inputs[key].data('title')
          var validState = inputs[key][0].validity

          if (!validState.valid) {
            if (validState.valueMissing)
              messages.push(title + ' is a required field')
            if (validState.badInput || validState.rangeUnderflow || validState.rangeOverflow)
              messages.push(title + ' contains invalid values')
          }
        }
      } else {
        messages.push('Upload a file')
      }

      displayErrors(messages)
    }

    return valid
  }

  /**
   * @function calculate
   * @summary Calls the calculation and displays results
   */
  function calculate () {
    if(validate()) {
      var url = 'calculate/'
      var model = data
      output = {}

      if (!inputs.description.val())
        inputs.description.val('Created ' + new Date().toLocaleString())

      if (inputs.manualReference.is(':checked'))
        model.reference = [
          +inputs.referenceAge.val(),
          +inputs.referencePeriod.val(),
          +inputs.referenceCohort.val()
        ]
      else
        delete model.reference

      // Disables input selection after calculation is made    
      $('#paste').prop('disabled', true) 
      $('#upload').prop('disabled', true)
      $('#file').prop('disabled', true)

      // Disables sample links
      $('.sampleLink').prop('disabled', true)

      // Add tooltip notifying user to clear input before calculating new data
      $('#input-panel').attr('title', 'Clear form before calcualting new data')

      $.post({
        url: url,
        data: JSON.stringify(model),
        beforeSend: function() {
          $('#loading').css('display', 'flex')
        },
        contentType: 'application/json',
        dataType: 'json',
        jsonp: false
      }).done(displayResults)
        .fail(displayError)
        .always(function() {
          $('#loading').css('display', 'none');
        })
    }
  }

  /**
   * @function displayError
   * @summary Displays any errors that occured during calculation
   * @param {Object} xhr
   * @param {Object} error
   * @param {string} statusText
   */
  function displayError (xhr, error, statusText) {
    var statuses = {
      503: 'The service that the request is trying to reach is currently down. Please try again later.',
      404: 'The request returned with a response of  "' + statusText + '". Please try again later.',
      400: xhr.responseText
    }

    displayErrors([statuses[xhr.status] || 'The request has failed for an unknown reason'])
  }

  /**
   * @function displayResults
   * @summary Displays calculation results
   * @param {Object} results The results object
   */
  function displayResults (results) {

    /** @type Object */
    output = results.output

    /** @type Object */
    var downloads = results.downloads

    for (key in output)
      $('#' + key).html(createPanel(key, output[key]))

    for (key in downloads)
      $('#' + key).prop('value', downloads[key])

    $('#download-results').show()
  }


  /**
   * @function createPanel
   * @summary Creates an html element for displaying results
   * @description
   * The content object contains the following properties:
   * {
   *  table {number|string[][]} The data table
   *  headers {string[]} Headers for the data table
   *  graph {string} The url to the generated graph
   * }
   *
   * @param {string} key The data key
   * @param {Object} content The contents of this panel
   * @returns {jQuery.div} A div containing panel information
   */
  function createPanel (key, content) {
    var panel = $('<div>')
    panel.addClass('text-center')

    if (content.graph) {
      var img = $('<img>')
      img.addClass('img-responsive')
      img.attr('alt', key)
      img.attr('src', content.graph)
      panel.append(img)

      // convert image to base64
      img[0].onload = function() {
        var canvas = document.createElement('canvas')
        var context = canvas.getContext('2d')
        canvas.height = this.height
        canvas.width = this.width
        context.drawImage(this, 0, 0)
        dataURL = canvas.toDataURL()
        output[this.alt].graph = dataURL
      }
    }

    var titles = {
      'NetDrift'     : 'Net Drift',
      'Waldtests'    : 'Wald Tests',
      'Coefficients' : 'Coefficients'
    }

    var table = DataTable.createOutput(content.table, content.headers, titles[key] || null)
    $(table).find('th').attr('role', 'column')
    panel.append(table)
    return panel
  }

  function getExcelData() {
    var keys = [
      'AgeDeviations',
      'PerDeviations',
      'CohDeviations',
      'LongAge',
      'CrossAge',
      'Long2CrossRR',
      'FittedTemporalTrends',
      'PeriodRR',
      'CohortRR',
      'LocalDrifts',
      'NetDrift',
      'Waldtests',
      'Coefficients'
    ]

    return keys.map(function(key) {
      return {
        title: key,
        table: [output[key].headers]
               .concat(output[key].table),
        image: output[key].graph || null
      }
    })
  }

  function displayErrors (errors) {
    $('#errors').empty()
    $('#errors').show()
    $('#errors').append(
      errors.map(function(error) {
        var msg = $('<div>')
        msg.text(error)
        return msg
      })
    )
  }
})()