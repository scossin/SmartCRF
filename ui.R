navbarPage("Cohorte n°67", 
           
           tabPanel("Recueil de données",
                    tags$head(
                      includeScript("www/js/orderDiv.js")),
                    includeScript("www/js/timelineClicked.js"),
                    includeScript("www/js/getAvailableDocuments.js"),
                    includeScript("www/js/displayId.js"),
                    includeScript("www/js/toRedcap.js"),
                    includeScript("www/js/seeDocument.js"),
                    includeCSS("www/css/css.css"),
                    #includeCSS("www/css/fontawesome-free-5.0.10/fontawesome-free-5.0.10/web-fonts-with-css/css/fontawesome.min.css"),
                    includeScript("https://use.fontawesome.com/releases/v5.0.10/js/all.js"),
                    
                    tags$script("$(document).on('click', '.modifyButton', function () {
                        var randomNumber = Math.random();
                                Shiny.onInputChange('modifyButton',this.id + '_' + randomNumber);
                                });"),
                    
                    tags$script(HTML(
                      "$(document).on('click', '#canvas', function() {",
                      'word = $("#wordcloudwcSpan")[0].innerHTML;',
                      "Shiny.onInputChange('wordcloudclicked', word);",
                      "});"
                    )),
                    
                    div(id = "firstDiv"),
                    
wordcloud2::wordcloud2Output("wordcloud", width = "30%", height = "400px"),

                    div (id = GLOBALdrugsDiv),

                    div(id = "Focus",
                        
                        # shiny::tag$h5("Focur sur : "),
                        shiny::actionButton(inputId = "showModalPMSIid",
                                            label="PMSI",
                                            icon = icon("eye"),
                                            style="font-size:1.5em;
                                                  margin-bottom:20px;
                                                  background-color:#ffd9b3;"),
                        shiny::actionButton(inputId = "showModalBIOLOGIEid",
                                            label="Biologie",
                                            icon = icon("eye"),
                                            style="font-size:1.5em;
                                                  margin-bottom:20px;
                                                  background-color:#ffccdd;"),
                        div(id = "timelineQ"
                            )
                        
                    )
                    
           )
)
