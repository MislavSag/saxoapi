#' @title SaxoAuthService Class
#'
#' @description
#' OPerates with Saxo Open API.
#'
#' @export
SaxoAuthService = R6::R6Class(
  classname = "SaxoAuthService",

  public = list(

    #' @field app_config Sax APP object
    app_config = NULL,

    #' @description Create a new AuthService object with provided AppConfig.
    #' When initialized, config is loaded either directly from app_config
    #' argument or from "app_config.json".
    #'
    #' @param app_config Sax APP object.
    #'
    #' @return A new `SaxoAuthService` object.
    initialize = function(app_config = NULL) {

      #
      if (file.exists("app_config.json")) {
        lg$info("found config file 'app_config.json'")
        self$app_config <- fromJSON("app_config.json")
      } else if (!is.null(app_config)) {
        self$app_config <- app_config
      } else {
        stop("no app config object found - make sure'app_config.json' is available in this directory or load the config directly when initializing SaxoAuthService")
      }
    },

    #' @description Login to Sax Open API
    #'
    #' @param redirect_url Redirect url
    #' @param redirect_port Redirect port
    #'
    #' @return Object that can get token.
    login = function(redirect_url = NULL, redirect_port = NULL) {

      lgl$info(
        paste0("logging in to app: ",  self$app_config$AppName,
               " using ", self$app_config$GrantType)
      )

      # defaults to first redirect url in config if not provided explicitly
      if (self$app_config$GrantType == "Code") {
        if (is.null(redirect_url)) {
          self$auth_redirect_url <- self$app_config$RedirectUrls[1]
        } else if (is.null(redirect_url) & is.null(self$app_config$RedirectUrls)) {
          stop("provided redirect url not valid for app config and won't be accepted by Saxo SSO")
        } else {
          self$auth_redirect_url <- redirect_url
        }
      }

      # TODO: add here PKCE later if necessary

      # log
      lg$info(
        paste0("redirect url for callback: ",  self$auth_redirect_url)
      )

      # define state variable
      state <- httr2:::base64_url_rand(24)

      # define client
      client <- oauth_client(
        id = self$app_config$AppKey,
        secret = self$app_config$AppSecret,
        token_url = self$app_config$TokenEndpoint,
        name = self$app_config$AppName
      )

      # auth url; url where the user is sent
      auth_url <- oauth_flow_auth_code_url(client,
                                           auth_url = self$app_config$AuthorizationEndpoint,
                                           redirect_uri = self$auth_redirect_url,
                                           scope = NULL,
                                           state = state
      )

      # open browser
      lg$info(paste0("browser will be opened with url: ", auth_url))
      browseURL(user_url)
    }
  )
)


# HELP, DELETE LATER

# self = list(
#   app_config = fromJSON("app_config.json")
# )
# self$app_config

# # Define logger
# tf <- tempfile()
# lg <- get_logger("saxapi")
# lg$add_appender(AppenderFile$new(tf))
