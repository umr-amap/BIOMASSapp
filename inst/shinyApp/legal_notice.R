##%######################################################%##
#                                                          #
####                    Legal notice                    ####
#                                                          #
##%######################################################%##

#terms of uses, Legal Notice (Informations/mentions légales), Credits (Crédits)

# UI CSS style
legalNoticeCSS <- function() {
  htmltools::singleton(inlineCSS("
    footer.legal_notice {
      position:fixed;
      bottom:0;
      width:100%;
      //height:25px;
      color: white;
      background-color: #222d32;
        z-index: 1000;
      text-align: center;
    }
"))
}

# UI notice that floats at bottom
legalNotice <- function(year, owner) {
  tagList(
    legalNoticeCSS(),
    tags$footer(class="legal_notice fixed-bottom",
      span(glue::glue("Creative Commons, {year}, {owner}")),br(),
      actionLink("legal_notice", "Legal notice")
    )
  )
}

# UI helper function to add footer to a dashboard page
dashboardAddFooter <- function(page, footer) {
  htmltools::tagAppendChild(page, footer, "div.wrapper")
}

# SERVER
legalNoticeHandler <- function(notice, title="Legal notice", size="m", session=shiny::getDefaultReactiveDomain()) {
  observeEvent(session$input$legal_notice, {
    showModal(modalDialog(
      size = size,
      title = title,
      notice
    ))
  })

}
