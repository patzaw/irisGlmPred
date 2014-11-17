library(shiny)
shinyUI(pageWithSidebar(
	headerPanel("Simple glm predictors of Iris species"),
	sidebarPanel(
    h3("Feed the predictor"),
		selectInput(
			inputId="feature",
			label="1) Select the feature to perform the prediction",
			choices=c(
				"Sepal length"="Sepal.Length",
				"Sepal width"="Sepal.Width",
				"Petal length"="Petal.Length",
				"Petal width"="Petal.Width"
			),
			selected="Petal.Length"
		),
		uiOutput("valSlider"),
    uiOutput("valSel"),
    p("... or click on the graph."),
    h3("Interpret the results"),
    p(
      "The higlited species is the most probable one according to the provided value.
      The prediction responses are displayed on the right of the graph."
    ),
		p(
		  "The curves correspond to the predictive functions for each species."
		)
	),
	mainPanel(
		plotOutput("modPlLeg", width="100%", height="100px"),
		plotOutput("modPlot", width = "100%", height="600px", clickId = "mpClick")
	)
))
