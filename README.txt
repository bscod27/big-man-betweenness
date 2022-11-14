Scripts:
	build.R - takes the raw input files, assembles networks, calculates measures of interest, creates weekly 'builds' of data for analysis
	batch_build.R - batch implementation of build.R which takes a command line argument that indicates the week of data to work on (enables parallel computation)
	analysis.R - takes the builds, wrangles them into analysis-friendly form, and produces the results found in the notebook
	gifs.R - script used to save down the individual images in the GIFs
	use_case.R - script used to produce the data for the use case within the football field visual


Folders:
	data - contains raw data as supplied the NFL Big Data Bowl competition 
	builds - contains the engineered data produced by builds.R
	snippets - contains intermediate data that is shown in report.ipynb
	images - contains the images produced produced by analysis.R 
	gifs - contains the images for the GIFS produced produced by gifs.R 
	use_case - contains data for the coordinates of players produced by analysis.R for the football field visual example


Other: 
	report.ipynb - submitted notebook containing the main findings
	model_results.xlsx - contains the formatted results from multilevel models 