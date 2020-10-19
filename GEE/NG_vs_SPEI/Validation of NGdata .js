//This script was used to visualize wether the NG and other krieged datasets
// were valid. It computes Kurtosis and skewness of the datasets giving a plot
// of those variables. It also computes the total area where the values have 
//been abnormal. Finally the timeseries of normalized values was also plotted.
//-------------------------------------------------------------------------//
//                              Data inputs                                //
//-------------------------------------------------------------------------//
var state = ['California'];
var usstate = ee.FeatureCollection("TIGER/2018/States");//us state vector
var stateshape = usstate.filter(ee.Filter.inList('NAME', state));// us state
var roi = stateshape.geometry();// us state//Define region of interest here
var ecoregion = ee.FeatureCollection("users/felipef93/Ecocal_project").filter(ee.Filter.inList('NA_L3NAME', ['Sierra Nevada','Klamath Mountains','Southern and Baja California Pine-Oak Mountains','Eastern Cascades Slopes and Foothills','Coast Range','Cascades','Northern Basin and Range','Central Basin and Range'] ));
var NG = ee.ImageCollection('users/felipef93/NETGrowth');

//-------------------------------------------------------------------------//
//                    Display Skewness and kutosis                         //
//-------------------------------------------------------------------------//
//Locations with skewness above absolute value of 2
var skew = NG.reduce(ee.Reducer.skew()).clip(roi);
var skewmask = skew.updateMask(skew.pow(2).gt(4)).clip(ecoregion).clamp(1,1).multiply(ee.Image.pixelArea()).divide(1E6);
//Locations divided by Kurtosis values: bellow 3; from 3 to 5; and above 5
var kurt = NG.reduce(ee.Reducer.kurtosis()).clip(ecoregion);
var image02 = kurt.lt(3).selfMask().clamp(3,3).unmask(0);
var image04 = kurt.gte(3).and(kurt.lt(5)).selfMask().clamp(4,4).unmask(0);
var image06 = kurt.gte(5).selfMask().clamp(5,5).unmask(0);
var kurtpal = image02.add(image04).add(image06).selfMask();

var band_viz = {
  min:3,
  max: 5,
  opacity: 1.0,
  palette: [ "green","yellow","red"]
};

var band_viz2 = {
  opacity: 1.0,
  palette: ["red"]
};

Map.addLayer(kurtpal,band_viz, "kurtosis");
Map.addLayer(skewmask,band_viz2, "skewness");

//-------------------------------------------------------------------------//
//                    Calculate area skew and kurt                         //
//-------------------------------------------------------------------------//
print("Total area of ecoregions in km2",ecoregion.geometry().area().divide(1e6));
var skewarea= skewmask.reduceRegion({
                            reducer:ee.Reducer.sum(),
                            geometry: ecoregion,
                            maxPixels: 1e10,
                            scale: 2500});
                            

var skewarea = ee.Number(skewarea.get("b1_skew"));
print("Areas with skewness >2",skewarea);

var kurtarea = kurt.lt(3).selfMask().clamp(1,1).multiply(ee.Image.pixelArea()).divide(1E6).reduceRegion({
                                      reducer:ee.Reducer.sum(),
                                      geometry: ecoregion,
                                      maxPixels: 1e10,
                                      scale: 2500});
var kurtarea = ee.Number(kurtarea.get("b1_kurtosis"));
print("Areas with kurtosis <3",kurtarea);
//-------------------------------------------------------------------------//
//                    NG variation in the western Cordillera               //
//-------------------------------------------------------------------------//
//Ecoregions of western cordillera only
var ecoregions = ee.FeatureCollection("users/felipef93/Ecocal_project").filter(ee.Filter.inList('NA_L3NAME', ['Sierra Nevada','Klamath Mountains','Eastern Cascades Slopes and Foothills','Cascades'] ));
//normalization of NG values
var meanNG = NG.reduce(ee.Reducer.mean());
var stdNG = NG.reduce(ee.Reducer.stdDev());
var NGcorrected = NG.map(function(image){
  var newimg= image.subtract(meanNG).divide(stdNG).clamp(-2,2);
  return newimg.set('year', image.get('system:index')).clip(roi);
});

var options = {
          title: 'Images',
          curveType: 'function',
          scale: 'continous',
          legend: { position: 'bottom' },
          colors:["#80342d","#714320","#48600c","#515151"]
        };

var chart = ui.Chart.image.seriesByRegion({
                        imageCollection: biomasscorrected,
                        regions: ecoregions,
                        reducer: ee.Reducer.mean(),
                        scale:5000,
                        band: 0,
                        xProperty: 'year',
                        seriesProperty:"NA_L3NAME"})
                        .setOptions(options);
print(chart);