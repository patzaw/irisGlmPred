## Title: Simple GLM models of the Iris data set
## Author: Patrice Godard

## Requirements
library(shiny)
library(RColorBrewer)
data(iris)

## Plotting tools
bw <- 0.6
iris$id <- 1:nrow(iris)
iris$jitter <- runif(n=nrow(iris), min=-bw/2, max=bw/2)
features <- setdiff(colnames(iris), "Species")

## Plotting one predictive feature
predFeatPlot <- function(val=NA, feature, iris.models, iris.pred, features, bw, selIds){
	## Features for the predictions
	featLab <- paste(gsub("[.]", " ", features), "(cm)")
	names(featLab) <- features
	##
	species <- sort(unique(iris.pred$Species))
	specCol <- brewer.pal(n=length(species), name="Set3")
	specPch <- rep(16,3)
	names(specPch) <- names(specCol) <- species
	## Predicted specy
	if(is.na(val) | val < min(iris.pred[,feature])){
		sel <- ""
		val <- NA
	}else{
		newdata <- data.frame(val)
		colnames(newdata) <- feature
		pred <- lapply(iris.models, predict, newdata=newdata, type="response")
		pred <- lapply(pred, function(x){names(x) <- c(); return(x)})
		pred <- unlist(pred)
		sel=names(pred)[which.max(pred)]
	}
	## Boxplot
	toPlot <- unstack(iris.pred, formula(paste(feature, "~", "Species")))
	par(mar=c(5.2,6.2,0.1,4.1))
	boxplot(
		toPlot,
		boxwex=bw,
		col=ifelse(names(toPlot)==sel, specCol[sel], "#BEBEBE50"),
		horizontal=T,
		xlab=featLab[feature],
		outline=F,
		ylim=range(unlist(toPlot)),
		names=rep("", length(toPlot))
	)
	mtext(
		names(toPlot),
		at=1:3,
		side=2,
		line=0.7,
		las=2,
		col=ifelse(names(toPlot)==sel, "black", "grey"),
		cex=ifelse(names(toPlot)==sel, 1.2, 0.8)
	)
	abline(
		h=(1:(length(toPlot)-1))+0.5,
		lty=2, lwd=3, col="grey"
	)
	## Adding points
	for(spi in 1:length(toPlot)){
		sp <- names(toPlot)[spi]
		curPl <- iris.pred[which(iris.pred$Species==sp),]
		points(
			curPl[,feature],
			curPl[,"jitter"]+spi,
			pch=specPch[sp],
			col=ifelse(
				rep(sp!=sel, nrow(curPl)),
				ifelse(
					sel==curPl[,"Spec.Pred"],
					"darkred",
					"grey"
				),
				ifelse(
					sel==curPl[,"Spec.Pred"],
					"darkblue",
					"grey"
				)
			)
		)
    curPl <- curPl[which(curPl$id %in% selIds),]
    if(nrow(curPl) > 0){
      points(
        curPl[,feature],
        curPl[,"jitter"]+spi,
        pch=21,
        col=ifelse(
          rep(sp!=sel, nrow(curPl)),
          ifelse(
            sel==curPl[,"Spec.Pred"],
            "darkred",
            "grey"
          ),
          ifelse(
            sel==curPl[,"Spec.Pred"],
            "darkblue",
            "grey"
          )
        ),
        cex = 2
      )
    }
	}
	## Adding predictions
	for(spi in 1:length(toPlot)){
		sp <- names(toPlot)[spi]
		points(
			iris.pred[order(iris.pred[,sp]),feature],
			iris.pred[order(iris.pred[,sp]),sp]*0.8+spi-0.4,
			type="l",
			lwd=2,
			col=ifelse(sp==sel, "black", "grey")
		)
		## Prediction legends
		# text(
		# 	rep(par()$usr[2], 2),
		# 	c(0.6, 1.4)+ spi - 1,
		# 	"-",
		# 	col=ifelse(sp==sel, "black", "grey"),
		# 	cex=2
		# )
		# mtext(
		# 	text=0:1,
		# 	side=4,
		# 	line=0.3,
		# 	col=ifelse(sp==sel, "black", "grey"),
		# 	at=c(0.6, 1.4)+ spi - 1,
		# 	cex=0.6
		# )
	}
	mtext(
		text="Prediction responses",
		side=4,
		line=2.5
	)
	## Showing the predicted value
	if(!is.na(val)){
		abline(v=val, col="black", lwd=2, lty=3)
		for(spi in 1:length(toPlot)){
			sp <- names(toPlot)[spi]
			segments(
				x0=val, y0=spi-0.5,
				x1=val, y1=pred[sp]*0.8+spi-0.4,
				lty=3,
				lwd=2,
				col=ifelse(sp==sel, "black", "grey")
			)
			segments(
				x0=val, y0=pred[sp]*0.8+spi-0.4,
				x1=par()$usr[2], y1=pred[sp]*0.8+spi-0.4,
				lty=3,
				lwd=2,
				col=ifelse(sp==sel, "black", "grey")
			)
			mtext(
				text=round(pred[sp],2),
				side=4,
				line=0.3,
				col=ifelse(sp==sel, "black", "grey"),
				at=pred[sp]*0.8+spi-0.4,
				cex=ifelse(sp==sel, 1.2, 0.8),
				las=2
			)
		}
	}
}

## The server
shinyServer(
	function(input,output, session){
		r.feature <- reactive(input$feature)
		## The slider
		output[["valSlider"]] <- renderUI({
			sliderInput(
				"val", "2) Select a value for prediction (in cm)...",
				min = min(iris[,r.feature()])-0.1,
				max = max(iris[,r.feature()]),
				value = min(iris[,r.feature()])-0.1,
				step=0.1,
				animate=F
			)
		})
		output[["valSel"]] <- renderUI({
		  numericInput(
		    "valField", "...or type it (in cm)...",
		    value = min(iris[,r.feature()])-0.1,
        step=0.1
		  )
		})
    observe({
      updateSliderInput(session, "val", value=input$valField)
    })
		observe({
		  updateNumericInput(session, "valField", value=input$val)
		})
		## Graph interactivity
		indSel <- reactiveValues(ids=NULL, formerClick=NULL)
		observe({
			# Initially will be empty
		  feature <- r.feature()
      newClick <- input$mpClick
		  if(is.null(input$mpClick) | identical(indSel$formerClick, newClick)){
		    return()
		  }
      indSel$formerClick <- newClick
      updatedVal <- round(newClick$x, digits=1)
			updateSliderInput(session, "val", value=updatedVal)
			updateNumericInput(session, "valField", value=updatedVal)
			indSel$ids <- iris$id[which(iris[,feature]==updatedVal)]
		})
		## Preparing the different models
		r.iris.models <- reactive({
			species <- sort(unique(iris$Species))
			feature <- r.feature()
			iris.models <- lapply(
				species,
				function(s){
					tmp <- iris
					tmp$cl <- tmp$Species==s
					glm(
						formula=formula(paste(
							"cl ~ ",
							'`', feature, '`',
							sep=""
						)),
						data=tmp,
						family=binomial
					)
				}
			)
			names(iris.models) <- species
			return(iris.models)
		})
		r.iris.pred <- reactive({
			feature <- r.feature()
			iris.pred <- as.data.frame(do.call(cbind, lapply(
				r.iris.models(),
				predict,
				type="response"
			)))
			iris.pred$Spec.Pred <- apply(
				iris.pred,
				1,
				function(x){
					colnames(iris.pred)[which.max(x)]
				}
			)
			iris.pred <- cbind(iris[,c("Species", feature, "jitter", "id"), drop=F], iris.pred)
			return(iris.pred)
		})
		## The graph
		output[["modPlot"]] <- renderPlot({
			species <- unique(iris$Species)
			feature <- r.feature()
			iris.models <- r.iris.models()
			iris.pred <- r.iris.pred()
			## Plotting
			val <- input$val
			if(is.null(val)) val <- NA
			predFeatPlot(
				val=val,
				feature=feature,
				iris.models=iris.models,
				iris.pred=iris.pred,
				features=features,
				bw=bw,
        selIds=indSel$ids
			)
		})
		## Graph legend
		output[["modPlLeg"]] <- renderPlot({
			feature <- r.feature()
			iris.models <- r.iris.models()
			iris.pred <- r.iris.pred()
			val <- input$val
			if(is.null(val)) val <- NA
			if(!is.na(val) & val >= min(iris.pred[, feature])){
				actPred <- T
			}else{
				actPred <- F
			}
			##
			par(mar=c(0.1, 6.2, 0.1, 4.1))
			plot(
				1,1,
				type="n", bty="o",
				xaxt="n", yaxt="n",
				xlab="", ylab="",
				main=""
			)
			##
# 			species <- sort(unique(iris.pred$Species))
# 			specCol <- brewer.pal(n=length(species), name="Set3")
# 			specPch <- 15:17
# 			names(specPch) <- names(specCol) <- species
# 			legend(
# 				ifelse(actPred, "top", "center"),
# 				legend=species,
# 				pch=specPch,
# 				col="black",
# 				bty="n",
# 				horiz=T,
# 				title="Actual species",
#         title.col="darkgreen",
#         cex=1
# 			)
			if(actPred){
# 				abline(h=1)
				legend(
					"center",
					legend=c("True positive", "False positive", "Negative"),
					pch=19,
					col=c("darkblue", "darkred", "grey"),
					bty="n",
					horiz=T,
					title="Predictions",
          title.col="darkgreen",
          cex=1
				)
			}
		})
	}
)
