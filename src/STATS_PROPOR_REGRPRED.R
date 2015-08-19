#Licensed Materials - Property of IBM
#IBM SPSS Products: Statistics General
#(c) Copyright IBM Corp. 2014
#US Government Users Restricted Rights - Use, duplication or disclosure 
#restricted by GSA ADP Schedule Contract with IBM Corp.

# Author: JKP, IBM SPSS
# Version = 1.0.1

# history
# 09-25-Sep-2013 - original version



helptext="This procedure calculates predicted values using
a model produced by STATS PROPOR REGRPRED and new data.

STATS PROPOR REGRPRED DATASET=dataset name
	PREDTYPE = RESPONSE or LINK or PRECISION or VARIANCE
	SOURCE= FILE or WORKSPACE
	FILE = 'filespec'
	ID = idvariable
/OPTIONS
	MISSING = MISSING or OMIT or STOP
	DISPLAYMODEL = NO or YES
/HELP


The DATASET and either SOURCE=WORKSPACE or FILE=filespec are required.

Example:
STATS PROPOR REGRPRED DATASET=predictions FILE='c:/temp/mymodel.Rdata'.

DATASET specifies the name for a new dataset to hold the predicted values.
The name must not already be in use.

PREDTYPE specifies the desired type of prediction.  The default is RESPONSE.

SOURCE specifies where the model is found.  If SOURCE=WORKSPACE, the model
comes from executing STATS PROPOR REGR in the current session with
RETAIN=YES.  If SOURCE=FILE, the model is loaded from the FILE
specification, which must have been saved with the RFILE keyword
in STATS PROPOR REGR.  If the FILE keyword is specified, it is
not necessary to also specify SOURCE.

ID specifies a variable to be included in the prediction dataset to
facilitate matching back to the input data.

MISSING specifies whether a missing value in any input variable produces
a case with a missing value for the prediction in the dataset, causes 
the case to be omitted, or causes the procedure to stop.

DISPLAYMODEL causes the saved model to be displayed.

STATS PROPOR REGRPRED /HELP prints this help and does nothing else.
"

propredext <- function(dataset, id=NULL, predtype="response", msource=NULL,
	filespec=NULL, missingv="missing", includeind=FALSE, displaymodel=FALSE
	) {
	
	allargs = as.list(environment())  # for passing external spec to other functions

    setuplocalization("STATS_PROPOR_REGRPRED")

    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output
    procname=gtxt("Proportional Variable Regression Prediction")
    warningsprocname = gtxt("Proportional Variable Regression Prediction: Warnings")
    omsid="STATSPROPREDREG"
    warns = Warn(procname=warningsprocname,omsid=omsid)

    tryCatch(library(betareg, quietly=TRUE), error=function(e){
        warns$warn(gtxtf("The R %s package is required but could not be loaded.","betareg"),
            dostop=TRUE)
    }
    )

	naaction = list(missing=na.pass, omit=na.omit, stop=na.fail)[[missingv]]
	dscheck(dataset, warns)
	getmodel(filespec, msource, warns)
	
	# get data requirement from model and retrieve data
	allvars = c(stats_propor_allargs[["indep"]], stats_propor_allargs[["indeppre"]], 
		stats_propor_allargs[["offsetv"]])
	if (!is.null(id) && id %in% allvars) {
		warns$warn(gtxt("The ID variable cannot be a predictor in the model"), dostop=TRUE)
	}
	allvars = c(id, allvars)
	displaymodel(stats_propor_allargs, allargs, stats_propor_res, stats_propor_ressum, warns)
	tryCatch(
		{
		dta = spssdata.GetDataFromSPSS(allvars, row.label=id, missingValueToNA=TRUE,
			factorMode="labels")
		},
		error = function(e) {
			warns$warn(gtxt("An input or ID variable was not found in the dataset"), dostop=TRUE)
		}
	)

	pred = tryCatch(
		predict(stats_propor_res, newdata=dta, type=predtype,
		na.action = naaction),
		error=function(e) {warns$warn(e$message, dostop=TRUE)
		}
	)
	createdataset(pred, dta, allargs)

    # clean up workspace
    if (!msource == "workspace") {
        res <- tryCatch(rm(list=ls()), warning = function(e) {return(NULL)})
    } else {
		rm(dta, pred)
	}
}

getmodel = function(filespec, msource, warns) {
	# load model workspace
	# filespec is a file name for the workspace to load
	# msource indicates whether a workspace or a file should be used

	if (is.null(msource)) {
		if (!is.null(filespec)) {
			msource = "file"
		} else {
			warns$warn(gtxt("No source specification was provided for the model"), dostop=TRUE)
		}
	}
    if (!xor(msource == "workspace", !is.null(filespec))) {
        warns$warn(gtxt("Either a workspace file or the use-retained option but not both must be specified"), dostop=TRUE)
    }
        
    # If model is from a file, load it now
    if (!is.null(filespec)) {
		tryCatch(
			load(file=filespec),
			error=function(e){
				warns$warn(gtxtf("The model file %s could not be found or is  not a valid model file.", filespec),
					dostop=TRUE)
			}
		)
    }
    # Is it a valid workspace?
    if (!(exists("stats_propor_res") && exists("stats_propor_allargs") 
        && exists("stats_propor_ressum"))) {
        warns$warn(gtxt("One or more required objects are missing from the specified workspace"),
            dostop=TRUE)
    }
	if (class(stats_propor_res) != "betareg" || class(stats_propor_ressum) != "summary.betareg") {
		warns$warn(gtxt("One or more proportional regression objects are invalid"), dostop=TRUE)
	}
}

displaymodel = function(info, allargs, res, res2, warns) {

    StartProcedure(gtxt("Proportional Variable Regression Prediction"), "STATSPROPREDREG")
    
    # summary results
	# input specifications
	lbls = c(gtxt("Dependent Variable"),
		gtxt("Link Function"),
		gtxt("Mean Model Variables"),
		gtxt("Precision Model Link Function"),
		gtxt("Precision Model Variables"),
		gtxt("Offset"),
		gtxt("Missing Value Treatment"),
		gtxt("Estimator Type"),
		gtxt("Convergence"),
		gtxt("Saved Workspace"),
		gtxt("Creation Date"),
		gtxt("Prediction Dataset")
	)

	etype = switch(res$type, 
		"ML" = gtxt("Maximum Likelihood"),
		"BC" = gtxt("ML Bias Corrected"),
		"BR" = gtxt("ML Bias Reduced")
	)

    vals = c(info["dep"],
		info["link"],
		paste(info[["indep"]], collapse=", "),
		info["plink"],
		ifelse(is.null(info[["indeppre"]]), gtxt("--NA--"), paste(info[["indeppre"]], collapse=", ")),
		ifelse(is.null(info[["offsetv"]]), gtxt("--NA--"), info["offsetv"]),
		info[["missingv"]],
		etype,
		ifelse(res["converged"], gtxt("Yes"), gtxt("No")),
		ifelse(is.null(info[["rfile"]]), gtxt("--NA--"), info[["rfile"]]),
		info[["modeldate"]],
		allargs[["dataset"]]
    )
	
    # settings and result summary
    spsspivottable.Display(data.frame(cbind(vals), row.names=lbls), title = gtxt("Summary"),
        collabels=c(gtxt("Summary")), templateName="PROSUMMARY", outline=gtxt("Summary"),
		caption = gtxt("Computations done by R package betareg")
	)

	#coefficients table
	if (allargs[["displaymodel"]]) {
		ctable = data.frame(coef(res2)$mean)
		cnames = c(gtxt("Coefficient"), gtxt("Std. Error"), gtxt("Z Value"), gtxt("Sig."))
		names(ctable) = cnames
		spsspivottable.Display(ctable, title=gtxt("Mean Coefficients"),
			templateName="PROMEANCOEF", outline=gtxt("Mean Coefficients")
		)
		ctable = data.frame(coef(res2)$precision)
		names(ctable) = cnames
		spsspivottable.Display(ctable, title=gtxt("Precision Coefficients"),
			templateName="PROPRECOEF", outline=gtxt("Precision Coefficients")
		)
	}
    warns$display(inproc=TRUE)
}

createdataset = function(pred, dta, details) {
    # Create residuals and/or predicted values dataset
    # Dataset name is known to be okay, and procedure state is ended
	# res is the summary results
	# res2 is the betareg object (non-summary)
	# dta is the data
	# iddata is the id variable (optional)
	# details is all the input arguments


	# construct a data frame with the requested variables
	# If the dataset has missing values, the predict and residuals output will have
	# No values for those, but they will carry the ID value, so we have to
	# pick up the ID values from the row names in the generated data frame and
	# copy them to the ID variable for sending back to Statistics
	# n.b. The doc for predict and residuals claims that these values are
	# present but NA, but the facts are otherwise.
	
	theframe = data.frame(id=names(pred), pred)

	if (!is.null(details[["id"]])) {  # was an id variable provided
		vardict = spssdictionary.GetDictionaryFromSPSS(details["id"])
		loc = match(details[["id"]], vardict["varName",])
		idtype = as.integer(vardict[["varType", loc]])
		idformat = vardict[["varFormat", loc]]
		idlabel = vardict[["varLabel", loc]]
		if (idlabel == "") {  # if no label, use the id variable name as the label
			idlabel = details[["id"]]
		}
	} else {
		idtype = 0
		idformat = "F10.0"
		idlabel = ""
	}
	dictspec = list(c("ID", idlabel, idtype, idformat, "nominal"))
	dictspec[2] = list(c("PredictedValues", 
		gtxtf("Predicted Values, Type=%s", details["predtype"]),
		0, "F8.4", "scale"))

	dict = spssdictionary.CreateSPSSDictionary(dictspec)
	spssdictionary.SetDictionaryToSPSS(details[["dataset"]], dict)
	spssdata.SetDataToSPSS(details[["dataset"]], theframe)
	spssdictionary.EndDataStep()

}

dscheck = function(alldsspecs, warns) {
	# check dataset validation conditions
	
    if (!is.null(alldsspecs)) {
        alldatasets = spssdata.GetDataSetList()
        if ("*" %in% alldatasets) {
            warns$warn(gtxt("The active dataset must have a name when creating new datasets"),
                dostop=TRUE)
        }
        if (length(intersect(alldsspecs, alldatasets) > 0)) {
            warns$warn(gtxt("One or more specified output dataset names are already in use"),
                dostop=TRUE)
        }
    }
}

	
# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}
gtxt <- function(...) {
    return(gettext(...,domain="STATS_PROPOR_REGRPRED"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_PROPOR_REGRPRED"))
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

    if (is.null(msg) || dostop) {
        lcl$display(inproc)  # display messages and end procedure state
        if (dostop) {
            stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
        }
    }
}

    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

    if (lcl$msgnum == 0) {   # nothing to display
        if (inproc) {
            spsspkg.EndProcedure()
        }
    } else {
        if (!inproc) {
            procok =tryCatch({
                StartProcedure(lcl$procname, lcl$omsid)
                TRUE
                },
                error = function(e) {
                    FALSE
                }
            )
        }
        if (procok) {  # build and display a Warnings table if we can
            table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
            rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

    for (i in 1:lcl$msgnum) {
        rowcategory = spss.CellText.String(as.character(i))
        BasePivotTable.SetCategories(table,rowdim,rowcategory)
        BasePivotTable.SetCellValue(table,rowcategory, 
            spss.CellText.String(lcl$msglist[[i]]))
    }
    spsspkg.EndProcedure()   # implies display
} else { # can't produce a table
    for (i in 1:lcl$msgnum) {
        print(lcl$msglist[[i]])
    }
}
}
}
return(lcl)
}

Run<-function(args){
    
    cmdname = args[[1]]
    args <- args[[2]]
    oobj<-spsspkg.Syntax(templ=list(
        spsspkg.Template("DATASET", subc="",  ktype="varname", var="dataset"),
		spsspkg.Template("ID", subc="", ktype="existingvarlist", var="id", islist=FALSE),
		spsspkg.Template("PREDTYPE", subc="", ktype="str", var="predtype",
			vallist = list("response", "link", "precision", "variance")),	
		spsspkg.Template("SOURCE", subc="", ktype="str", var="msource",
			vallist = list("file", "workspace")),
		spsspkg.Template("FILE", subc="", ktype="literal", var="filespec"),
			
        spsspkg.Template("MISSING", subc="OPTIONS",  ktype="str", 
            var="missingv", vallist=list("missing", "omit", "stop")),
        spsspkg.Template("INCLUDEIND", subc="OPTIONS",  ktype="bool", var="includeind"),
		spsspkg.Template("DISPLAYMODEL", subc="OPTIONS", ktype="bool", var="displaymodel")
    ))        
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    } else {
        res <- spsspkg.processcmd(oobj,args,"propredext")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
