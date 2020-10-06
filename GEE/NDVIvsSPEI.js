/*/
This script uses MODIS 1km NDVI product and it is spatially correlated
with SPEI calculated from NOAH Global Land Assimulation System data. 
It will display and export the correlation map of SPEI vs three months 
sum of NDVI anomalies.
Pixels with p-value <0.05 will be filtered out in the correlation map.
This study further analyzes the correlation spatial pattern by computing
the average R in different koppen climate zones. (extreme dry climate not 
included)
(SPEIxMonth in Jan-Dec vs NDVI three month anomalies)                  
//------------------------------------------------------------------------//
// For fast global study                                                  
// Contact: Shunan Feng (冯树楠): fsn.1995@gmail.com                      
*/

//------------------------------------------------------------------------//
//                               Datainput                                //
//------------------------------------------------------------------------//

var ndvi = ee.ImageCollection("NOAA/CDR/AVHRR/NDVI/V5") //Noah Ndvi data from 1981 to 2019
    .select('NDVI');


var spei= ee.ImageCollection("GRIDMET/DROUGHT");

//------------------------------------------------------------------------//
//                                 Inputs                                 //
//------------------------------------------------------------------------//
//Location:
var state = ['California']; //State of interest (only works for US)

//Period:
var year_start = 1982; //Year start
var year_end = 2019; //Year end

//SPEI:
var speim = 3;  // Month of SPEI to be analysed
//NDVI:
var ndvi_range = 9; //Program will compute the sum of ndvi anomalies for a period of "ndvi_range"(in months)
var ndvi_lag = 2; // 0 corresponding to a ndvi_anomaly_sum strating in the month of speim
                 // + values = ndvi is lagged  and - values = spei is lagged
var yearextentNDVI = 1 //Extent of years of the variable NDVI. i.e: if analysis takes place from march of a year
					   //to february of the next year extent is 1 year 
var yearlagSPEI = 0 //Variable that indicates if  SPEI in the correlations is lagged by year. 
					// ie: If analysing NDVI of march comparing with spei of november from previous year, yearlagSPEI=1


//------------------------------------------------------------------------//
//                             Preparation Inputs                         //
//------------------------------------------------------------------------//
var dummyndvi = ndvi_range-12*yearextentNDVI;
var dummyspei = speim-12*yearlagSPEI;
var month_start = dummyspei +ndvi_lag;
var month_end = dummyspei + ndvi_lag+ dummyndvi-1; 

var usstate = ee.FeatureCollection("TIGER/2018/States");//us state vector
var stateshape = usstate.filter(ee.Filter.inList('NAME', state));// us state

var date_start = ee.Date.fromYMD(year_start, 1, 1);
var date_end = ee.Date.fromYMD(year_end, 12, 31);
var years = ee.List.sequence(year_start, year_end);// time range of years
var months = ee.List.sequence(1, 12);// time range of months
var spei_m = ee.List.sequence(speim,12);

var clip = function(image){
  var clipped = image.clipToCollection(stateshape);
  return clipped;
};
var spei = ee.ImageCollection(spei.map(clip)).filterDate(date_start, date_end);
var ndvi = ee.ImageCollection(ndvi.map(clip)).filterDate(date_start, date_end);
//------------------------------------------------------------------------//
//                             Preparation NDVI                           //
//------------------------------------------------------------------------//
//Mean montlhy NDVI

var NDVI_monthlyave = ee.ImageCollection.fromImages(
    years.map(function (y) {
        return months.map(function(m) {
            var vi = ndvi       .filter(ee.Filter.calendarRange(y, y, 'year'))
                                .filter(ee.Filter.calendarRange(m, m, 'month'))
                                .median()
                                .rename('NDVIm');
            return vi.set('year', y)
                     .set('month', m)
                     .set('system:time_start', ee.Date.fromYMD(y, m, 1));
        });
    }).flatten()
);

// 30yr monthly average NDVI
var NDVI_30yrave = ee.ImageCollection.fromImages(
    months.map(function (m) {
        var vi = NDVI_monthlyave.filter(ee.Filter.eq('month', m))
                                .mean()
                                .rename('NDVIy');
        return vi.set('month', m);
    }).flatten()
);

//Anomaly calculation
var monthfilter = ee.Filter.equals({
    leftField: 'month',
    rightField: 'month',
});
var monthlink = ee.Join.saveFirst({
    matchKey: 'match',
});
var NDVI_monthlink = ee.ImageCollection(monthlink.apply(NDVI_monthlyave,NDVI_30yrave,monthfilter))
    .map(function(image) {
        return image.addBands(image.get('match'));
    });

//Function to sum the NDVI anomalies months
var addNDVI_anomaly = function(image) {
    var anomaly = image.expression(
        'b1-b2',
        {
            b1: image.select('NDVIm'),
            b2: image.select('NDVIy'),
        }
    ).rename('NDVI_anomaly');
    return image.addBands(anomaly);
};
                            
var NDVI_anomaly = NDVI_monthlink.map(addNDVI_anomaly);

// Month sum of NDVI_anomaly 
var NDVI_anomaly_sum = ee.ImageCollection.fromImages(
    years.map(function (y) {
        var startDate = ee.Date.fromYMD(y, month_start, 1);
        var endDate = ee.Date.fromYMD(ee.Number(y).add(yearextentNDVI), month_end, 2);
        var vi = NDVI_anomaly.select('NDVI_anomaly')
                             .filterDate(startDate, endDate)
                             .sum()
                             .rename('NDVI_anomaly_sum');
        return vi.set('year', y)
                 .set('month', speim)// here is set as the month of spei (April)
                 .set('system:time_start', ee.Date.fromYMD(y, speim, 1));
    }).flatten()
);

//------------------------------------------------------------------------//
//                             Preparation SPEI                           //
//------------------------------------------------------------------------//

var SPEI_monthly = ee.ImageCollection.fromImages(
    years.map(function (y) {
        return spei_m.map(function(m) {
            var vi = spei       .filter(ee.Filter.calendarRange(y, y, 'year'))
                                .filter(ee.Filter.calendarRange(m, m, 'month'))
                                .mean()
                                .clamp(-3,3);
            return vi.set('year', y)
                     .set('month', m)
                     .set('system:time_start', ee.Date.fromYMD(y, m, 1).advance(yearlagSPEI,'year'));
        });
    }).flatten()
);

//Selection of bands that I will be working
var spei1m= SPEI_monthly.select('spei30d');
var spei3m=SPEI_monthly.select('spei90d');
var spei6m=SPEI_monthly.select('spei180d');
var spei9m=SPEI_monthly.select('spei270d');
var spei12m=SPEI_monthly.select('spei1y');
var spei24m=SPEI_monthly.select('spei2y');

//------------------------------------------------------------------------//
//                            Comparison SPEI/NDVI anomalies              //
//------------------------------------------------------------------------//

var yearfilter = ee.Filter.equals({
    leftField: 'system:time_start',
    rightField: 'system:time_start',
});

var yearlink = ee.Join.saveFirst({
    matchKey: 'match',
});

var NDVI_spei1m = ee.ImageCollection(yearlink.apply(NDVI_anomaly_sum.select('NDVI_anomaly_sum'),
        spei1m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });
var NDVI_spei3m = ee.ImageCollection(yearlink.apply(NDVI_anomaly_sum.select('NDVI_anomaly_sum'),
        spei3m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });

var NDVI_spei6m = ee.ImageCollection(yearlink.apply(NDVI_anomaly_sum.select('NDVI_anomaly_sum'),
        spei6m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });

var NDVI_spei9m = ee.ImageCollection(yearlink.apply(NDVI_anomaly_sum.select('NDVI_anomaly_sum'),
        spei9m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });

var NDVI_spei12m = ee.ImageCollection(yearlink.apply(NDVI_anomaly_sum.select('NDVI_anomaly_sum'),
        spei12m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });

var NDVI_spei24m = ee.ImageCollection(yearlink.apply(NDVI_anomaly_sum.select('NDVI_anomaly_sum'),
        spei24m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });
        
//------------------------------------------------------------------------//
//                          Export correlation maps    			          //
//------------------------------------------------------------------------//
var corrmap1m = NDVI_spei1m.reduce(ee.Reducer.pearsonsCorrelation());

print(corrmap1m);
Map.addLayer(corrmap1m);
var corrmap3m = NDVI_spei3m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap6m = NDVI_spei6m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap9m = NDVI_spei9m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap12m = NDVI_spei12m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap24m = NDVI_spei24m.reduce(ee.Reducer.pearsonsCorrelation());

Export.image.toAsset({
  image: corrmap1m,
  description: 'corr',
  assetId: 'users/felipef93/CP3/',
  scale: 2500,
});

Export.image.toAsset({
  image: corrmap3m,
  description: 'corr',
  assetId: 'users/felipef93/CP3/',
  scale: 2500,
});

Export.image.toAsset({
  image: corrmap6m,
  description: 'corr',
  assetId: 'users/felipef93/CP3/',
  scale: 2500,
});

Export.image.toAsset({
  image: corrmap9m,
  description: 'corr',
  assetId: 'users/felipef93/CP3/',
  scale: 2500,
});

Export.image.toAsset({
  image: corrmap12m,
  description: 'corr',
  assetId: 'users/felipef93/CP3/',
  scale: 2500,
});

Export.image.toAsset({
  image: corrmap24m,
  description: 'corr',
  assetId: 'users/felipef93/CP3/',
  scale: 2500,
});