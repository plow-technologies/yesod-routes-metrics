
# 0.3.2.0 -- 2018.05.01
* upgrade to ghc 8.2.2, lts-11.7, tests building

# 0.3.1.0 -- 2017.09.27
* Add `resetYesodMetricsCounters` function.

# 0.3.0.0 -- 2017.09.08

* Add `Yesod.Routes.Stats` to calculate a percentile value for a `Vector Double`.

* Collect new metrics for each Yesod routes: `routesTotalRequests`, 
  `routeMaxLatencies`, `routeMinLatencies`, `routeAverageLatencies`, 
  `route50thPercentiles`, `route75thPercentiles`, `route90thPercentiles`, 
  `route99thPercentiles`.

* Add tests for new metrics.

* `registerYesodMetricsMkMetricsFunction` now returns 
  `IO (YesodMetrics, Middleware)` in case you want to monitor the values in 
  `YesodMetrics` manually.

# 0.2.0.0 -- 2017.09.08

* Add `YesodMetricsConfig` to allow control over certain features of the route 
  names.

* Add `defaultYesodMetricsConfig`, `spacedYesodMetricsConfig`, 
  `addSpacesToRoute` functions.

* Alter various functions to require `YesodMetricsConfig`.

* Add `registerYesodMetricsMkMetricsFunction` to simplify initialization and 
  make sure `registerYesodMetrics`and `metrics` use the same data.

* Create app in tests to make sure route names and store work as expected with 
  different options.
