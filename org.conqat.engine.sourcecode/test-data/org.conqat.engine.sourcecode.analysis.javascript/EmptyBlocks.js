// No missing braces finding
this.client.getAnalysisProfileNames(this.getBulkCallback(goog.bind(function(
		names) {
	this.analysisProfiles = names;
}, this)));

// Should trigger a finding
for(var i = 0; i<10;i++)
	i++;