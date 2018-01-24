# cbcr-frontend
 
[![Build Status](https://travis-ci.org/hmrc/cbcr-frontend.svg)](https://travis-ci.org/hmrc/cbcr-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/cbcr-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/cbcr-frontend/_latestVersion)


## Summary

This Webapp is the frontend to Country-by-Country-Reporting. It provides authenticated journeys to facilitate the 
uploading of financial reports for multinational companies with a gross turnover in excess of £750M.

## Dependencies

This service relies on the [CBCR](https://github.com/hmrc/cbcr) protected service for access to its domain and forms part of its bounded context. 

## Running

Use default JVM settings

```sbtshell
sbt 'run -DwhiteListDisabled=true -Dapplication.router=testOnlyDoNotUseInAppConf.Routes'
```

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")

### Submission Metadata

A sample submission metadata json can be found [here](conf/docs/metadata.json)
