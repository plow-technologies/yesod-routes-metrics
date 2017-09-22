# 0.3.0.0 -- 2017.09.08

* Add `Yesod.Routes.Stats` to calculate a percentile value for a `Vector Double`.

* Collect new metrics for each Yesod routes: `routesTotalRequests`, 
  `routeMaxLatencies`, `routeMinLatencies`, `routeAverageLatencies`, 
  `route50thPercentiles`, `route75thPercentiles`, `route90thPercentiles`, 
  `route99thPercentiles`.

* Add tests for new metrics.

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