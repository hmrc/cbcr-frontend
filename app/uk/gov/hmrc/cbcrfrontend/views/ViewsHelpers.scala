package uk.gov.hmrc.cbcrfrontend.views

import javax.inject.Inject

import uk.gov.hmrc.play.views.html.helpers._
import uk.gov.hmrc.play.views.html.layouts._
class ViewsHelpers @Inject()(
                              //copied from uk.gov.hmrc.play.views.html.helpers
                              val address:                    Address,
                              val dateFields:                 DateFields,
                              val dateFieldsFreeInline:       DateFieldsFreeInline,
                              val dateFieldsFreeInlineLegend: DateFieldsFreeInlineLegend,
                              val dateFieldsFreeYearInline:   DateFieldsFreeYearInline,
                              val dateFieldsFreeYear:         DateFieldsFreeYear,
                              val dateFieldsInline:           DateFieldsInline,
                              val dropdown:                   Dropdown,
                              val errorInline:                ErrorInline,
                              val errorNotifications:         ErrorNotifications,
                              val errorSummary:               ErrorSummary,
                              val fieldGroup:                 FieldGroup,
                              val form:                       FormWithCSRF,
                              val input:                      Input,
                              val inputRadioGroup:            InputRadioGroup,
                              val reportAProblemLink:         ReportAProblemLink,
                              val singleCheckbox:             SingleCheckbox,
                              val textArea:                   TextArea,

                              //copied from uk.gov.hmrc.play.views.html.layouts
                              val article:           Article,
                              val attorneyBanner:    AttorneyBanner,
                              val betaBanner:        BetaBanner,
                              val footer:            Footer,
                              val euExitLinks:       EuExitLinks,
                              val footerLinks:       FooterLinks,
                              val head:              Head,
                              val headerNav:         HeaderNav,
                              val loginStatus:       LoginStatus,
                              val mainContent:       MainContent,
                              val mainContentHeader: MainContentHeader,
                              val optimizelySnippet: OptimizelySnippet,
                              val gtmSnippet:        GTMSnippet,
                              val serviceInfo:       ServiceInfo,
                              val sidebar:           Sidebar
                            )
