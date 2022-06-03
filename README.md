# saxoapi

This repository helps developers working with Saxo's OpenAPI using R.

The package provides following features:
1. Authentification using Oauth 2.0. Code Grant Flow
2. Account data
3. Place orders
4. Get info on positions.

In the future, I plan to add additional API endpoints:

# Prerequisit

To use Ouath 2.0 Code Grant Flow you should:
1. save APP OBJECT in app_config.json file or
2. Provide APP OBJECT json file as argument to `login` method.


Usage:
```
library(saxoapi)

# Saxo Open API Token
saxo = SaxoAuthService$new()
tokens <- saxo$login()

# API endpoints
saxo$get_user()
client <- saxo$get_client()
account <- saxo$get_accounts()
balances <- saxo$get_account_balance(account_key = account$Data[[1]]$AccountKey,
                                     client_key = client$ClientKey)
instruments <- saxo$get_instruments(keywords = "SPY", 
                                    asset_types = NULL, 
                                    include_nontradable = TRUE)
body_params = list(
  AccountKey = account$Data[[1]]$AccountKey,
  Uic = "36590",
  BuySell = "Buy",
  AssetType = "Etf",
  Amount = 100,
  OrderType = "Market",
  ManualOrder = TRUE,
  OrderDuration = list(DurationType = "DayOrder")
)
order_id <- saxo$place_order(body_params)
saxo$get_orders_all()
saxo$get_positions(client$ClientKey)
```


