# yesod-routes-metrics

yesod-routes-metrics provides middleware to track data for routes access. The 
main function is `registerYesodMetrics`.

## Examples

- Simple app with metrics - https://github.com/plow-technologies/yesod-routes-metrics/tree/master/examples/print-routes

- Print out all metrics - https://github.com/plow-technologies/yesod-routes-metrics/tree/master/examples/yesod-with-metrics

## How to use yesod-routes-metrics

```haskell
module Yesod.Routes.Metrics where

registerYesodMetrics :: Bool -> Text -> ByteString -> Store -> IO YesodMetrics
registerYesodMetrics verbose namespace routesFileContents store
```

#### verbose 

If `True`, it will create six counters per route.

```
/ HomeR GET
```

will result in six counters

```
getHomeR
getHomeR_response_status_1xx
getHomeR_response_status_2xx
getHomeR_response_status_3xx
getHomeR_response_status_4xx
getHomeR_response_status_5xx
```

otherwise if it is `False` it will only provide

```
getHomeR
```

which is total sum of all responses for a single path.

#### namespace

A string that will be appended to the name of each metric. This is useful if 
you have multiple servers sending data to the same source.

#### routesFileContents

Parse the Yesod routes file like this

```haskell
import           Data.ByteString (ByteString)
import           Data.FileEmbed

routesFileContents :: ByteString
routesFileContents = $(embedFile "config/routes")
```

and pass it to `registerYesodMetrics`.

#### store

```haskell
import System.Metrics -- ekg-core package

main = do
  ...
  store <- newStore
```

#### main

You can use `forkServerWith` from [ekg](https://hackage.haskell.org/package/ekg) 
or send data to [statsd](https://github.com/etsy/statsd) 
via [ekg-statsd](https://hackage.haskell.org/package/ekg-statsd). 
You can also make your own custom solution.

```haskell
import           System.Metrics           (newStore)
import qualified Yesod.Routes.Metrics as Yesod
import           System.Remote.Monitoring (forkServerWith) -- ekg package

main = do
  yesodMetrics <- Yesod.registerYesodMetrics True "routes" routesFileContents store
  registerGcMetrics store

  -- use this if you want to use the 
  _ <- forkServerWith store "localhost" 7000

  run 3000 (Yesod.metrics routesFileContents yesodMetrics $ app)
```

## Types of Routes Supported

Only certain types of Yesod routes are supported by yesod-routes-metrics. To 
fully support Dynamic routes would require Template Haskell to build the `convertMethodToRouteName`
function because it takes a String input and needs to reify Strings to type at
compile time. You are welcome to add this functionality and make a PR.

#### Fully Supported

- Static routes. The example below is actually two routes: `getHomeR` and `postHomeR`.

```
/             HomeR     GET POST
```

#### Semi-Supported

- Dynamic single routes will be caught but no type checking is performed. Get requests
to `/blog/1` and `/blog/hello` will be counted as metrics for `getBlogPostR`.

```
/blog/#BlogId BlogPostR GET
```

- Overlapped Dynamic single routes will only catch the first one because there is no
type checking. `/foo/1` and `/foo/hello` will be counted as metrics for `getFoo2R`.

```
!/foo/#Int  Foo2R GET
!/foo/#Text Foo3R GET
```

#### Unsupported

- Static routes are ignored.

```
/static       StaticR   Static getStatic
```

- Dynamic multi routes are also ignored.

```
/wiki/*Texts WikiR GET
```