.packageName <- "RcmdrPlugin.qcc"



thereispkg <- function(x,type="numeric") {
	if ( (type == "n") || (type == "numeric") || (type == 0) ) {
		.Allpack <- as.array(.packages(all.available = TRUE))
		if ( sum( x == .Allpack ) == 1 ) return(1)
		else return(0)
	}
	else {
		if ( (type == "c") || (type == "character") || (type == 1) ) {
			.Allpack <- as.array(.packages(all.available = TRUE))
			if ( sum( x == .Allpack ) == 1 ) return("exist")
			else return("doesntexist")
		}
		else return(cat("Error! Check type. \n"))
	}
}


checkpkg <- function(pkg) {
	if (thereispkg(pkg) == 0) {
		msg <- paste("Do you want to install '",pkg,"' package?",sep="")
		ans <- as.character(tkmessageBox(message=msg,icon="question",type="yesnocancel",default="yes"))
		if (ans == "yes") {
			doItAndPrint(paste("install.packages('",pkg,"',dependencies = TRUE)",sep=""))
			if ( thereispkg(pkg) == 0 ) {
				msg <- paste("SORRY! The '",pkg,"' package could not be installed. Try again.",sep="")
				ans <- tkmessageBox(message=msg,icon="warning",type="ok")
			}
		}
		else {
		msg <- paste("SORRY! You want to install '",pkg,"' package to use this feature.",sep="")
		ans <- tkmessageBox(message=msg,icon="warning",type="ok")
		}
	}
}





pareto <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("Pareto Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Defects:"))
    yBox <- variableListBox(top, title=gettextRcmdr("Frequencys:"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        names(y) <- x
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=pareto, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if ((length(x) != length(y)))
        	{
            errorCondition(recall=pareto, message=gettextRcmdr("The two variable must be the same length."))
            return()
        	}
        if (x == y){
            errorCondition(recall=pareto, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        justDoIt(paste("attach(",.activeDataSet,")"))
        justDoIt(paste("names(",y,") <- ",x,""))
        doItAndPrint(paste("pareto.chart(", y, ")", sep=""))
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbar <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("X-bar Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgXbar, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgXbar, message=gettextRcmdr("Sigma number must be positive."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("qcc(.Sampledata, type='xbar', nsigmas=",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgXbarCCO <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("X-bar Operation Characteristic Curve")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgXbarCCO, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("oc.curves(qcc(.Sampledata, type='xbar',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbarewma <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("X-bar EWMA Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgXbarewma, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda         <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgXbarewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgXbarewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata, type='xbar', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
	  tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbarcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("X-bar Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgXbarcusum, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata, type='xbar',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbarcapabi <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("X-bar Capability Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    inferiorVar <- tclVar("")
    inferiorEntry <- tkentry(especFrame, width="6", textvariable=inferiorVar)
	  superiorVar <- tclVar("")
	  superiorEntry <- tkentry(especFrame, width="6", textvariable=superiorVar)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgXbarcapabi, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}

        sigma          <- as.numeric(tclvalue(sigmaVar))
        inferior       <- as.numeric(tclvalue(inferiorVar))
		    superior       <- as.numeric(tclvalue(superiorVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgXbarcapabi, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (inferior >= superior)
        	{
            errorCondition(recall=cgXbarcapabi, message=gettextRcmdr("Upper Limit must be greater than Lower Limit."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("process.capability(object = qcc(.Sampledata, type='xbar', nsigmas=",sigma,"), spec.limits=c(",inferior,",",superior,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lower Limit:")), inferiorEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Upper Limit:")), superiorEntry, sticky="w")
	  tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgS <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("S Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgS, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgS, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}

        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("qcc(.Sampledata, type='S', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgSewma <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("S EWMA Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgSewma, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda         <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgSewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgSewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata, type='S', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
	  tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgScusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("S Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgScusum, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata, type='S',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgR <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("R Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgR, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgR, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}

        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("qcc(.Sampledata, type='R', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgRewma <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("R EWMA Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgRewma, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda         <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgRewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgRewma, message=gettextRcmdr("Lambda must be between 0 and1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata, type='R', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
	  tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgRcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("R Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, selectmode="multiple", title=gettextRcmdr("Select the sample variables")) # Define lista de variáveis com a amostra
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) <= 1)
        	{
            errorCondition(recall=cgRcusum, message=gettextRcmdr("You must select more than one sample variable."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata, type='R',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbarone <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("Individual Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) != 1)
        	{
            errorCondition(recall=cgXbarone, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgXbarone, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("qcc(.Sampledata, type='xbar.one', nsigmas =",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)
    tkgrid(getFrame(xBox),sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgXbaroneewma <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("Individual EWMA Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) != 1)
        	{
            errorCondition(recall=cgXbaroneewma, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda         <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgXbaroneewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgXbaroneewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata, type='xbar.one', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
	  tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbaronecusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("Individual Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) != 1)
        	{
            errorCondition(recall=cgXbaronecusum, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata, type='xbar.one',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox),sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgXbaronecapabi <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("Individual Capability Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    inferiorVar <- tclVar("")
    inferiorEntry <- tkentry(especFrame, width="6", textvariable=inferiorVar)
	  superiorVar <- tclVar("")
	  superiorEntry <- tkentry(especFrame, width="6", textvariable=superiorVar)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        if (length(x) != 1)
        	{
            errorCondition(recall=cgXbaronecapabi, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}

        sigma          <- as.numeric(tclvalue(sigmaVar))
        inferior       <- as.numeric(tclvalue(inferiorVar))
		    superior       <- as.numeric(tclvalue(superiorVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgXbaronecapabi, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (inferior >= superior)
        	{
            errorCondition(recall=cgXbaronecapabi, message=gettextRcmdr("O limite inferior deve ser maior do que o superior."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
       	doItAndPrint(paste("process.capability(object = qcc(.Sampledata, type='xbar.one', nsigmas=",sigma,"), spec.limits=c(",inferior,",",superior,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lower Limit:")), inferiorEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Upper Limit:")), superiorEntry, sticky="w")
	  tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgP <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("P Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgP, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgP, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgP, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("qcc(.Sampledata,.Samplesize, type='p', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)

    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}


cgPCCO <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("P Operation Characteristic Curve")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgPCCO, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgPCCO, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("oc.curves(qcc(.Sampledata,.Samplesize, type='p',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgPewma <- function() {
	checkpkg("qcc")
    require("qcc")
	  initializeDialog(title=gettextRcmdr("P EWMA Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgPewma, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgPewma, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda       <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgPewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgPewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata,.Samplesize, type='p', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
    tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgPcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("P Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgPcusum, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgPcusum, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata,.Samplesize, type='p',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgnP <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("nP Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgnP, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgnP, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgnP, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("qcc(.Sampledata,.Samplesize, type='np', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)

    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgnPCCO <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("nP Operation Characteristic Curve")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgnPCCO, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgnPCCO, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("oc.curves(qcc(.Sampledata,.Samplesize, type='np',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgnPewma <- function() {
	checkpkg("qcc")
    require("qcc")
	  initializeDialog(title=gettextRcmdr("nP EWMA Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgnPewma, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgnPewma, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda       <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgnPewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgnPewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata,.Samplesize, type='np', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
    tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgnPcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("nP Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgnPcusum, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgnPcusum, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata,.Samplesize, type='np',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgC <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("C Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgC, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgC, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgC, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("qcc(.Sampledata,.Samplesize, type='c', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)

    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgCCCO <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("C Operation Characteristic Curve")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgCCCO, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgCCCO, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,"[1]",sep=""))
       	doItAndPrint(paste("oc.curves(qcc(.Sampledata,.Samplesize, type='c',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgCewma <- function() {
	checkpkg("qcc")
    require("qcc")
	  initializeDialog(title=gettextRcmdr("C EWMA Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgCewma, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgCewma, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda       <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgCewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgCewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata,.Samplesize, type='c', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
    tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgCcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("C Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgCcusum, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgCcusum, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata,.Samplesize, type='c',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgU <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("U Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgU, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgU, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgU, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("qcc(.Sampledata,.Samplesize, type='u', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)

    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgUCCO <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("U Operation Characteristic Curve")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgUCCO, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgUCCO, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,"[1]",sep=""))
       	doItAndPrint(paste("oc.curves(qcc(.Sampledata,.Samplesize, type='u',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgUewma <- function() {
	checkpkg("qcc")
    require("qcc")
	  initializeDialog(title=gettextRcmdr("U EWMA Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgUewma, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgUewma, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda       <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgUewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgUewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata,.Samplesize, type='u', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
    tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgUcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("U Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgUcusum, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgUcusum, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata,.Samplesize, type='u',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgG <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("G Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    rightFrame <- tkframe(top)
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgG, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgG, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma <- tclvalue(sigmaLevel)
        if (sigma <= 0)
        	{
            errorCondition(recall=cgP, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
        	}

        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("qcc(.Sampledata,.Samplesize, type='g', nsigmas = ",sigma,",data.name =",.dataName,")", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    sigmaFrame <- tkframe(rightFrame)
    sigmaLevel <- tclVar("3")
    sigmaField <- ttkentry(sigmaFrame, width="6", textvariable=sigmaLevel)



    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid(labelRcmdr(sigmaFrame, text=gettextRcmdr("k =  ")), sigmaField, sticky="w")
    tkgrid(sigmaFrame, sticky="w")
    tkgrid.configure(sigmaField, sticky="e")
    dialogSuffix(rows=4, columns=2)
}

cgGewma <- function() {
	checkpkg("qcc")
    require("qcc")
	  initializeDialog(title=gettextRcmdr("G EWMA Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    especFrame <- tkframe(top)
    sigmaVar <- tclVar("3")
    sigmaEntry <- tkentry(especFrame, width="6", textvariable=sigmaVar)
    lambdaVar <- tclVar("0.2")
    lambdaEntry <- tkentry(especFrame, width="6", textvariable=lambdaVar)

    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgGewma, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgGewma, message=gettextRcmdr("Variables must be different."))
            return()
            }

        sigma          <- as.numeric(tclvalue(sigmaVar))
        lambda       <- as.numeric(tclvalue(lambdaVar))

        if (sigma <= 0)
        	{
            errorCondition(recall=cgGewma, message=gettextRcmdr("The number of sigmas must be positive."))
            return()
           	}
        if (0>= lambda || lambda > 1)
        	{
            errorCondition(recall=cgGewma, message=gettextRcmdr("Lambda must be between 0 and 1."))
            return()
           	}
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("ewma(qcc(.Sampledata,.Samplesize, type='g', nsigmas = ",sigma,",data.name =",.dataName,"), lambda = ",lambda,")", sep=""))
       	justDoIt(paste("rm(.Sampledata)"))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(top, text=gettextRcmdr("Specifications"), fg="blue"), sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("k = ")), sigmaEntry, sticky="w")
	  tkgrid(tklabel(especFrame, text=gettextRcmdr("Lambda = ")), lambdaEntry, sticky="w")
    tkgrid(especFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}

cgGcusum <- function() {
	checkpkg("qcc")
    require("qcc")
	initializeDialog(title=gettextRcmdr("G Cumulative Sum Control Chart")) 
    xBox <- variableListBox(top, title=gettextRcmdr("Select the sample variable"))
    yBox <- variableListBox(top, title=gettextRcmdr("Select the variable to samples size(s)"))
    onOK <- function()
    	{
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if ((length(x) != 1) || (length(y) != 1))
        	{
            errorCondition(recall=cgGcusum, message=gettextRcmdr("You must select at least one variable."))
            return()
        	}
        if (x == y){
            errorCondition(recall=cgGcusum, message=gettextRcmdr("Variables must be different."))
            return()
            }
        closeDialog()
        .dbdata <- ActiveDataSet()
        .seleCteds <- paste(x, collapse=",")
        .dataName <- paste("'",ActiveDataSet() ,"'",sep="")
        justDoIt(paste(".Sampledata <- subset(",.dbdata,",select=c(",.seleCteds,"))",sep=""))
        justDoIt(paste(".Samplesize <- ",.dbdata,"$",y,sep=""))
       	doItAndPrint(paste("cusum(qcc(.Sampledata,.Samplesize, type='g',data.name =",.dataName,"))", sep=""))
       	justDoIt(paste("rm(.Sampledata,.Samplesize)"))
       	tkdestroy(top)
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="qcc")
    rightFrame <- tkframe(top)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(tklabel(rightFrame, text=""), sticky="w")
    tkgrid(rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
}
