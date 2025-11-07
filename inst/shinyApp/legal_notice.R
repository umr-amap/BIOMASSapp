##%######################################################%##
#                                                          #
####                    Legal notice                    ####
#                                                          #
##%######################################################%##

#terms of uses, Legal Notice (Informations/mentions légales), Credits (Crédits)

# UI notice that floats at bottom
legalNoticeBslib <- function(year, owner) {

  tags$div(
    class = "mt-auto pt-4",
    style = "border-top: 1px solid rgba(255,255,255,0.2); font-size: 0.9em; text-align: center;",
    span(glue::glue("GPL 3 license, {year}, {owner}")),
    br(),
    actionLink("legal_notice", "Legal notice",
               style = "color: inherit; text-decoration: underline;")
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
