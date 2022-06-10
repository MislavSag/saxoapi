#' @title SaxoAuthService Class
#'
#' @description
#' OPerates with Saxo Open API.
#'
#' @export
SaxoAuthService = R6::R6Class(
  classname = "SaxoAuthService",

  public = list(

    #' @field app_config Saxo APP object
    app_config = NULL,

    #' @field auth_redirect_url Redirect URL for Saxo API.
    auth_redirect_url = NULL,

    #' @field auth_code Authentification code
    auth_code = NULL,

    #' @field token_data Token data
    token_data = NULL,

    #' @field base_request Base httr2 request object
    base_request = NULL,


    #' @description Create a new AuthService object with provided AppConfig.
    #' When initialized, config is loaded either directly from app_config
    #' argument or from "app_config.json".
    #'
    #' @param app_config Saxo APP object.
    #' @param auth_redirect_url Redirect URL for Saxo API.
    #' @param auth_code Authentification code.
    #' @param token_data Token data.
    #' @param base_request Base httr2 request.
    #'
    #' @return A new `SaxoAuthService` object.
    initialize = function(app_config = NULL, auth_redirect_url = NULL,
                          auth_code = NULL, token_data = NULL,
                          base_request = NULL) {

      # init
      self$auth_redirect_url <- NULL
      self$auth_code <- NULL
      self$token_data <- NULL
      self$base_request <- NULL

      # get APP OBJECT json file
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
    #' @param user_name User name for authentification in the browser.
    #'    NULL if you want to authenticate manually.
    #' @param password Password for authentification in the browser.
    #'    NULL if you want to authenticate manually.
    #'
    #' @return Object that can get token.
    login = function(redirect_url = NULL, redirect_port = NULL,
                     user_name = NULL, password = NULL) {

      ############ TEST ##########
      # library(jsonlite)
      # library(httr2)
      # library(chromote)
      # library(callr)
      # self = list(
      #   auth_redirect_url = "http://localhost:4321/redirect",
      #   app_config = fromJSON("app_config.json")
      # )
      ############ TEST ##########

      lg$info(
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

      # define client
      client <- oauth_client(
        id = self$app_config$AppKey,
        secret = self$app_config$AppSecret,
        token_url = self$app_config$TokenEndpoint,
        name = self$app_config$AppName
      )

      # define state variable
      state <- httr2:::base64_url_rand(24)

      # auth url; url where the user is sent
      auth_url <- oauth_flow_auth_code_url(client,
                                           auth_url = self$app_config$AuthorizationEndpoint,
                                           redirect_uri = self$auth_redirect_url,
                                           scope = NULL,
                                           state = state
      )

      # browser authentification
      if (is.null(user_name) | is.null(password)) {
        # start browser
        lg$info(paste0("browser will be opened with url: ", auth_url))
        utils::browseURL(auth_url)
      } else {
        rp <- r_bg(
          function(auth_url, user_name, password) {

            # start headless chrome session
            library(chromote)
            b <- ChromoteSession$new()
            b$Page$navigate(auth_url)
            Sys.sleep(3L)
            eval_ <- paste0('document.querySelector("#field_userid").value = "', user_name, '"')
            x <- b$Runtime$evaluate(eval_)
            Sys.sleep(1L)
            eval_ <- paste0('document.querySelector("#field_password").value = "', password, '"')
            x <- b$Runtime$evaluate(eval_)
            Sys.sleep(1L)
            x <- b$Runtime$evaluate('document.querySelector("#button_login").click()')
            Sys.sleep(1L)
            x <- b$Runtime$evaluate('document.querySelector("#page > div.container > div.form > div.validation-summary-errors > span").innerText')
            list(auth_url, user_name, password, eval_)
          },
          args = list(auth_url, user_name, password)
        )
      }

      # listen redirect url
      result <- self$my_oauth_flow_auth_code_listen()
      self$auth_code <- oauth_flow_auth_code_parse(result, state)

      # get tokens
      self$token_data <- self$exercise_authorization(self$auth_code)

      # get base request
      self$base_request <- self$get_base_request()

      return(self$token_data)
    },


    #' @description Starts an webserver that listens for the response from the resource server.
    #'
    #' @param host_ip IP address web server will be bound to.
    #' @param port Port to bind web server to.
    #'     By default, this uses a random port.
    #'     You may need to set it to a fixed port if the API requires that
    #'     the redirect_uri specified in the client exactly matches
    #'     the redirect_uri generated by this function.
    #' @param path Add path to listener
    #'
    #' @return Object that can get token.
    my_oauth_flow_auth_code_listen = function(host_ip = "127.0.0.1", port = 4321, path = "/redirect") {
      complete <- FALSE
      info <- NULL
      listen <- function(env) {
        if (!identical(env$PATH_INFO, path)) {
          return(list(status = 404L, headers = list(`Content-Type` = "text/plain"),
                      body = "Not found"))
        }
        query <- env$QUERY_STRING
        if (!is.character(query) || identical(query, "")) {
          complete <<- TRUE
        }
        else {
          complete <<- TRUE
          info <<- httr2:::parse_form_urlencoded(query)
        }
        list(status = 200L, headers = list(`Content-Type` = "text/plain"),
             body = "Authentication complete. Please close this page and return to R.")
      }
      server <- httpuv::startServer(host_ip, port, list(call = listen))
      withr::defer(httpuv::stopServer(server))
      lg$info("Waiting for authentication in browser...")
      lg$info("Press Esc/Ctrl + C to abort")
      while (!complete) {
        httpuv::service()
      }
      httpuv::service()
      if (is.null(info)) {
        stop("Authentication failed; invalid url from server.")
      }
      return(info)
    },

    #' @description Get token.
    #'
    #' @param auth_code Authorization code.
    #'
    #' @return Saxo Open API token.
    exercise_authorization = function(auth_code) {

      # make POST request
      resp <- request(self$app_config$TokenEndpoint) %>%
        req_body_form(
          grant_type = "authorization_code",
          code = auth_code,
          redirect_uri = self$auth_redirect_url,
          client_id = self$app_config$AppKey,
          client_secret = self$app_config$AppSecret
        ) %>%
        req_headers(
          "Host" = "sim.logonvalidation.net",
          "Accept" = "application/json"
        ) %>%
        req_method("POST") %>%
        req_perform()
      access_tokens <- resp %>% resp_body_json()

      # check for errors
      if (resp_status(resp) == 201) {
        lg$info("access & refresh token created/refreshed successfully")
      } else {
        stop("error occurred while attempting to retrieve token")
      }

      return(access_tokens)
    },

    #' @description Refresh token endpoint https://www.developer.saxo/openapi/learn/oauth-authorization-code-grant
    #'
    #' @param refresh_token Refresh token.
    #'
    #' @return Saxo Open API token.
    get_refreshed_token = function(refresh_token) {

      # make POST request
      resp <- request(self$app_config$TokenEndpoint) %>%
        req_body_form(
          grant_type = "refresh_token",
          refresh_token = refresh_token,
          redirect_uri = self$auth_redirect_url,
          client_id = self$app_config$AppKey,
          client_secret = self$app_config$AppSecret
        ) %>%
        req_headers(
          "Host" = "sim.logonvalidation.net"
          # "Authorization" = paste0("Basic ", ) # DELTE LATER IF NOT USED
        ) %>%
        req_method("POST") %>%
        req_perform()
      access_tokens <- resp %>% resp_body_json()

      # check for errors
      if (resp_status(resp) == 201) {
        lg$info("access & refresh token created/refreshed successfully")
      } else {
        stop("error occurred while attempting to retrieve token")
      }

      return(access_tokens)
    },

    #' @description Check token and prepare request
    #'
    #' @param tokens Saxo Open API tokens.
    #'
    #' @return Prepared httr2 request object.
    get_base_request = function() {

      if (is.null(self$token_data)) {
        stop("Generate Saxo Open API token first.")
      } else {
        req_base <- request(self$app_config$OpenApiBaseUrl) %>%
          req_headers("Authorization" = paste0("Bearer ", self$token_data$access_token))
      }
      return(req_base)
    },

    #' @description Help function to send get requests
    #'
    #' @param add_url Add url to base url.
    #'
    #' @return response from request.
    request_get_saxo = function(add_url) {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append(add_url) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    },

    #' @description User endpoint <https://www.developer.saxo/openapi/referencedocs/root/v1/user>
    #'
    #' @return User data.
    get_user = function() {
      self$request_get_saxo("port/v1/users/me")
    },

    #' @description Client Details endpoint <https://www.developer.saxo/openapi/referencedocs/port/v1/clients>
    #'
    #' @return Client Details data.
    get_client = function() {
      self$request_get_saxo("port/v1/clients/me")
    },

    #' @description Accounts endpoint <https://www.developer.saxo/openapi/referencedocs/port/v1/accounts>
    #'
    #' @return Accounts data.
    get_accounts = function() {
      self$request_get_saxo("port/v1/accounts/me")
    },


    # BALANCES ----------------------------------------------------------------
    # Url: https://www.developer.saxo/openapi/referencedocs/port/v1/balances

    #' @description Get balance data for logged-in client
    #'    More info on <https://www.developer.saxo/openapi/referencedocs/port/v1/balances>.
    #'
    #' @return Positions data.
    get_balances_user = function() {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("port/v1/balances/me") %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    },

    #' @description Account balance endpoint <https://www.developer.saxo/openapi/referencedocs/port/v1/balances>
    #'
    #' @param account_key Account key.
    #' @param client_key client key.
    #'
    #' @return Account Balance data.
    get_balance_account = function(account_key, client_key) {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("port/v1/balances") %>%
        req_url_query(AccountKey = account_key, ClientKey = client_key) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    },

    #' @description Instruments endpoint <https://www.developer.saxo/openapi/referencedocs/ref/v1/instruments>
    #'
    #' @param keywords Keywords for instrument.
    #' @param asset_types Asset types.
    #' @param include_nontradable Include nontradable instrument or not.
    #'
    #' @return Instruments Data.
    get_instruments = function(keywords = NULL,
                               asset_types = NULL,
                               include_nontradable = FALSE) {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("ref/v1/instruments") %>%
        req_url_query(Keywords = keywords,
                      AssetTypes = asset_types,
                      IncludeNonTradable = include_nontradable) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      # unnest Tradeable As and rbindlist using data.table
      instruments <- resp$Data
      instruments <- lapply(instruments, function(x) {
        x[["TradableAs"]] <- paste0(unlist(x[["TradableAs"]]), collapse = ",")
        x
      })
      instruments <- rbindlist(instruments, fill = TRUE)

      return(instruments)
    },

    #' @description Place New Order endpoint <https://www.developer.saxo/openapi/referencedocs/trade/v2/orders>
    #'
    #' @param body_params Body of POST request with request parameteres (list)
    #'
    #' @return Order ID.
    place_order = function(body_params = list()) {

      # checks
      req_names <- c("AccountKey", "Uic", "BuySell", "AssetType", "Amount", "OrderType")
      if (!all(req_names %in% names(body_params))) {
        missing_var <- setdiff(req_names, names(body_params))
        stop(paste0("body_params must contain", missing_var))
      }

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("trade/v2/orders") %>%
        req_body_json(body_params) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    },

    #' @description All Open Orders endpoint endpoint <https://www.developer.saxo/openapi/referencedocs/port/v1/orders>
    #'
    #' @param field_groups FieldGroups, list that can contain:
    #'      DisplayAndFormat - Adds information about the instrument,
    #'        which is useful for display and formatting.
    #'        This includes Currency Code, Decimals, Instrument Description,
    #'        Display Decimals, Price format and Symbol
    #'      ExchangeInfo - Adds information about the instrument's exchange.
    #'        This includes Exchange name, exchange code and open status.
    #'      Greeks - Greeks for Option(s), only applicable to Fx Options,
    #'        Contract Options and Contract options CFD
    #'
    #' @return Json object of all orders.
    get_orders_all = function(field_groups = NULL) {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("port/v1/orders/me") %>%
        req_url_query(fieldGroups = field_groups) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    },

    #' @description Get positions for a client, account group, account or a position.
    #'    More info on <https://www.developer.saxo/openapi/referencedocs/port/v1/positions>.
    #'
    #' @param client_key client key.
    #' @param field_group FieldGroups: DisplayAndFormat, ExchangeInfo, Greeks,
    #'    PositionBase, PositionIdOnly, PositionView.
    #'
    #' @return Positions data.
    get_positions = function(client_key = NULL, field_group = NULL) {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("port/v1/positions") %>%
        req_url_query(ClientKey = client_key, FieldGroups = field_group) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    },

    #' @description Get net positions for the current user's client.
    #'    More info on <https://www.developer.saxo/openapi/referencedocs/port/v1/netpositions>.
    #'
    #' @param top top.
    #' @param skip skip.
    #' @param field_group look at <https://www.developer.saxo/openapi/referencedocs/port/v1/netpositions>
    #'
    #' @return Positions data.
    get_net_positions_user = function(top = NULL, skip = NULL,
                                      field_group = "DisplayAndFormat") {

      # make get request
      resp <- self$base_request %>%
        req_url_path_append("port/v1/netpositions/me") %>%
        req_url_query(top = top, sip = skip, FieldGroups = field_group) %>%
        req_perform()

      if (resp_status(resp) == 200) {
        resp <- resp_body_json(resp)
      }

      return(resp)
    }


    #' #' @description Get positions for a client, account group, account or a position.
    #' #'    More info on <https://www.developer.saxo/openapi/referencedocs/port/v1/positions>.
    #' #'
    #' #' @param client_key client key.
    #' #' @param field_group FieldGroups: DisplayAndFormat, ExchangeInfo, Greeks,
    #' #'    PositionBase, PositionIdOnly, PositionView.
    #' #'
    #' #' @return Positions data.
    #' get_positions = function(client_key = NULL, field_group = NULL) {
    #'
    #'   # make get request
    #'   resp <- self$base_request %>%
    #'     req_url_path_append("port/v1/positions") %>%
    #'     req_url_query(ClientKey = client_key, FieldGroups = field_group) %>%
    #'     req_perform()
    #'
    #'   if (resp_status(resp) == 200) {
    #'     resp <- resp_body_json(resp)
    #'   }
    #'
    #'   return(resp)
    #' }

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
