library(shiny)

predoptions <- data.table()

shinyServer(
    function(input, output, clientData, session) {
        # Make the wordcloud drawing predictable during a session
        #wordcloud_rep <- repeatable(wordcloud)
        
        observe({
            # Process input
            usrinput <- input$usr_input
            pre_tokens <- unlist(strsplit(tokenize(usrinput), split=' '))
            if (grepl(' $', usrinput)) {
                # User typed an space
                if (length(pre_tokens) > 2) {
                    pre_tokens <- pre_tokens[(length(pre_tokens)-2):length(pre_tokens)]
                }
                if (length(pre_tokens) == 0) { pre_tokens = c() }
                predoptions <<- word_predictions(pre_tokens)
                updateRadioButtons(session, "pred_options",
                                   choices = predoptions[1:10, word],
                                   selected = character(0), inline=F)
            }
            else {
                # Provide auto-complete
                gpatt <- paste('^', pre_tokens[length(pre_tokens)], sep = '')
                choices <- grep(gpatt, predoptions[, word], value = T)[1:10]
                not_na <- sum(!is.na(choices))
                if (not_na < 10) {
                    if (not_na == 0) {choices <- c()} else {choices <- choices[1:not_na]}
                    choices <- c(choices, grep(gpatt, sub_unigrams[,token], value = T)[1:(10 - not_na)])
                }
                not_na <- sum(!is.na(choices))
                if (not_na == 0) {choices <- c()} else {choices <- choices[1:not_na]}
                updateRadioButtons(session, "pred_options",
                                   choices = choices,
                                   selected = character(0), inline=F)
            }
            
        })
        
        observe({
            # Allow user select from radio options and update input text
            usrinput <- isolate({input$usr_input})
            opt_selected <- input$pred_options
            if (!is.null(opt_selected)) {
                if (!grepl(' $', usrinput)) {
                    # Is on autocomplete mode, remove partial word typed
                    last_space <- sapply(gregexpr(" ", usrinput), tail, 1)
                    usrinput <- substring(usrinput, 0, last_space)
                }
                # Concatenate selection to input
                updateTextInput(session, 'usr_input', value=paste(usrinput, opt_selected, ' ', sep=''))
            }
        })
        
        cloud_terms <- reactive({
            input$ref_cloud
            isolate({ 
                withProgress({
                    setProgress(message = "Updating cloud...")
                    predoptions 
                })
            })
        })
            
        output$word_plot <- renderPlot({
            cterms <- cloud_terms()
            wordcloud(cterms[,word], cterms[,metric], scale=c(3, 0.5),
                          #min.freq = input$freq, 
                          max.words=input$max_words,
                          colors=brewer.pal(8, "Dark2"),
                          random.order=F)
        })
        
        output$rant_text <- reactive({
            input$rand_rant
            isolate({ 
                withProgress({
                    setProgress(message = "Preparing a new rant...")
                    sentence_maker_final(isolate({tokenize(input$rant_seed)}), size=50, top=5)
                })
            })
        })

})