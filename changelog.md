# 0.2.0.0 -- 2017.09.08

* Add `YesodMetricsConfig` to allow control over certain features of the route 
  names.

* Add `defaultYesodMetricsConfig`, `spacedYesodMetricsConfig`, 
  `addSpacesToRoute` functions.

* Alter various functions to require `YesodMetricsConfig`.

* Add `registerYesodMetricsMkMetricsFunction` to simplify initialization and 
  make sure `registerYesodMetrics`and `metrics` use the same data.