$(document).ready(function() {

	// Save DOM references to minimize queries 
	var cfg = crosstalk.config({
		description:	$('#description'),
		startYear:		$('#startYear'),
		startAge:		$('#startAge'),
		interval:		$('#interval'),
		titleA:			$('#title1'),
		titleB:			$('#title2'),
		inputFileA:		$('#inputfile1'),
		inputFileB:		$('#inputfile2'),
	});
	
	// Add event listeners
	for (element in cfg)
		cfg[element].change(crosstalk.update);
	
	$('#startYear').spinner({
        min: 1800,
        max: 2200,
        step: 1,
        spin: crosstalk.update,
        stop: crosstalk.update
    });

	$('#startAge').spinner({
        min: 0,
        max: 120,
        step: 1,
        spin: crosstalk.update,
        stop: crosstalk.update
    });

	$('#interval').spinner({
        min: 1,
        max: 10,
        step: 1,
        spin: crosstalk.update,
        stop: crosstalk.update
    });

	$('#process').click(crosstalk.log);
});

/* ---------- ReadFile Module - Parses an input file as a crosstalk model ---------- */
var readfile = (function() {

	return {
		createModel: createModel
	}
	
	/* ---------- Reads a file from an input element and returns the generated model as a parameter to the callback ---------- */
	function createModel(element, callback) {
		readFile(element, function(contents) {
			callback(element.id, {
				title: 			parseHeader(contents.shift()),
				description: 	parseHeader(contents.shift()),
				startYear: 		parseInt(parseHeader(contents.shift())),
				startAge: 		parseInt(parseHeader(contents.shift())),
				interval: 		parseInt(parseHeader(contents.shift())),
				table: 			parseTable(contents)
			});
		});
	}

	/* ---------- Reads a file from an input element and returns an array of lines to the callback ---------- */
	function readFile(element, callback) {
		if (window.FileReader) {
			var file = element.files[0];
			var reader = new FileReader();

			if (file)
				reader.readAsText(file);

			reader.onload = function(event) {
				callback(event.currentTarget.result.match(/[^\r\n]+/g));
			}
		}
	}

	/* ---------- Parse an array of lines (from a csv) as a matrix of floats ---------- */
	function parseTable(table) {
		return table.map(function(line) {
			return line.split(',').map(function(element) {
				return parseFloat(element);
			});
		});
	}

	/* ---------- Parse a header line - return the portion after the first colon ---------- */
	function parseHeader(line) {
		var description = line.match(/"(.*?)"/);

		if (description)
			line = description[1];
		else
			line = line.split(',')[0];

		return line.split(/:(.+)?/, 2)[1].trim();
	}

})();


/* ---------- CrossTalk Module - Syncs the model and calculation results with the UI ---------- */
var crosstalk = (function($, ReadFile) {
	var self = this;
	
	// Holds DOM references
	self.cfg = {
		description:	null,
		startYear:		null,
		startAge:		null,
		interval:		null,
		titleA:			null,
		titleB:			null,
		inputFileA:		null,
		inputFileB:		null,
	}

	// Holds values from the DOM in a model
	self.model = {
		description: 	null,
		startYear: 		null,
		startAge: 		null,
		interval: 		null,
		titleA: 		null,
		titleB: 		null,
		inputfile1: 	null,
		inputfile2: 	null,
	}

	self.modeltable = {
		a: null,
		b: null
	}

	return {
		calculate:	calculate,
		update:		update,
		config: 	config,
		log: 		log,
	}

	function calculate() {
		console.log('JSON object for calculations: ' + self.model);

	}

	/* ---------- Updates the UI and Model ---------- */
	function update() {
		var self = this;

		// Updates the model based on the contents of the file
		if (self.type == 'file')
			ReadFile.createModel(self, updateModel);

		else
			syncModel();
	}

	/* ---------- Updates the model with contents from the UI ---------- */
	function syncModel() {
		self.model.description = self.cfg.description.val();
		self.model.startYear = self.cfg.startYear.val();
		self.model.startAge = self.cfg.startAge.val();
		self.model.interval = self.cfg.interval.val();
		self.model.titleA = self.cfg.titleA.val();
		self.model.titleB = self.cfg.titleB.val();
	}

	/* ---------- Saves the file contents to the model ---------- */
	function updateModel(id, contents) {
		self.model[id] = contents;

		self.cfg.startAge.val(self.model[id].startAge);
		self.cfg.startYear.val(self.model[id].startYear);
		self.cfg.interval.val(self.model[id].interval);

		if (self.model.inputfile1)
			self.cfg.titleA.val(self.model.inputfile1.title);

		if (self.model.inputfile2)
			self.cfg.titleB.val(self.model.inputfile2.title);

		syncModel();
	}

	function updateTable(id, contents) {

		$('#table1').html('');

		var table = document.createElement('table');

		headers = self.model.inputfile1.table[0];

		table.setAttribute('id', id);
		table.setAttribute('class', 'table display compact');
		table.setAttribute('width', '100%');

		$('#table1').append(table);

		var headers = []

		for (var i = 0; i < self.model.inputfile1.table[0].length / 2; i ++)
			headers.push({title: (i).toString()})


		$('#tableA').DataTable({
			'destroy': true,
			'columns': headers,
			'data': self.model.inputfile1.table,
			'bSort': false,
			'bFilter': false,
			'paging': false,
			'responsive': true,
			'dom': 't'
		});

	}

	/* ---------- Saves DOM references in the configuration ---------- */
	function config(cfg) {
		self.cfg = cfg;
		return self.cfg;
	}

	/* ---------- Logs the current configuration and model to the console ---------- */
	function log() {

		console.log('Configuration', self.cfg);
		console.log('Model', self.model);
		updateTable('tableA', self.model.inputfile1.table);
	}

})($, readfile);

