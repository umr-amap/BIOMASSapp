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
      position:absolute;
      bottom:0;
      width:100%;
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
    tags$footer(class="legal_notice",
      span(glue::glue("GPL 3 license, {year}, {owner}")),br(),
      actionLink("legal_notice", "Legal notice")
    )
  )
}

# UI helper function to add footer to a dashboard page
dashboardAddFooter <- function(page, footer, where=c("page", "sidebar")) {
  where <- match.arg(where)
  htmltools::tagAppendChild(page, footer, ifelse(where=="page", "div.wrapper", "div.wrapper>aside"))
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
