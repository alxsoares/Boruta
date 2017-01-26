# Two functions converting Boruta result into a convenient formula,
# for instance for testing or passing to classification algorithms.
# Author: Miron B. Kursa
###############################################################################

##' Export Boruta result as a formula
##'
##' Functions which convert the Boruta selection into a formula, so that it could be passed further to other functions.
##' @param x an object of a class Boruta, made using a formula interface.
##' @return Formula, corresponding to the Boruta results.
##' \code{getConfirmedFormula} returns only Confirmed attributes, \code{getNonRejectedFormula} also adds Tentative ones.
##' @note This operation is possible only when Boruta selection was invoked using a formula interface.
##' @rdname getFormulae
##' @export
getConfirmedFormula<-function(x){
	if(!inherits(x,'Boruta'))
	 stop('This function needs Boruta object as an argument.');
	if(is.null(x$call[["formula"]]))
	 stop('The model for this Boruta run was not a formula.');
	deparse(x$call[["formula"]][[2]])->dec;
	preds<-paste(names(x$finalDecision)[x$finalDecision=='Confirmed'],collapse="+");
	return(as.formula(sprintf('%s~%s',dec,preds)));
}

##' @rdname getFormulae
##' @export
getNonRejectedFormula<-function(x){
	if(!inherits(x,'Boruta'))
	 stop('This function needs Boruta object as an argument.');
	if(is.null(x$call[["formula"]]))
	 stop('The model for this Boruta run was not a formula.');
	deparse(x$call[["formula"]][[2]])->dec;
	preds<-paste(names(x$finalDecision)[x$finalDecision!='Rejected'],collapse="+");
	return(as.formula(sprintf('%s~%s',dec,preds)));
}
