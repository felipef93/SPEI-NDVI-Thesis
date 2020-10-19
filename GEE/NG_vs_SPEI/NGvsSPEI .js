
//------------------------------------------------------------------------//
//                                 Inputs                                 //
//------------------------------------------------------------------------//

var state = ['California'];
var usstate = ee.FeatureCollection("TIGER/2018/States");//us state vector
var stateshape = usstate.filter(ee.Filter.inList('NAME', state));// us state
var roi = stateshape.geometry();

//Period:
var year_start = 2006; //Year start
var year_end = 2018; //Year end

//SPEI:
var speim = 3; // Month of SPEI to be analysed
var yearlag=1; //Lag beetween SPEI and NG

//------------------------------------------------------------------------//
//                       Initialization Variables                         //
//------------------------------------------------------------------------//

var year_start = year_start-yearlag; //Representation of lag in SPEI
var years = ee.List.sequence(year_start, year_end);// Needed to map SPEI
var spei_m = ee.List.sequence(speim, 12); //Needed to map SPEI

//------------------------------------------------------------------------//
//                             Preparation Biomass                        //
//------------------------------------------------------------------------//

////////////////////////////IF PEARSON CORRELATION//////////////////////////
//The NG values computed in R are standardized and clamped to values
//beetween -2 and 2

var biomass = ee.ImageCollection('users/felipef93/NETGrowth'); //NG dataset
var meanbiomass = biomass.reduce(ee.Reducer.mean());
var stdbiomass = biomass.reduce(ee.Reducer.stdDev());
var biomasscorrected = biomass.map(function(image){
  var newimg= image.subtract(meanbiomass).divide(stdbiomass);
  return newimg.clip(roi).clamp(-2,2);
});

////////////////////////////IF KENDALL CORRELATION//////////////////////////
//No procedure needed for Kendall as it is a rank correlation
/*
var biomasscorrected = ee.ImageCollection('users/felipef93/NETGrowth').map(clip);
*/
//------------------------------------------------------------------------//
//                             Preparation SPEI  	                      //
//------------------------------------------------------------------------//
//Values are divided to represent a monthly period (mean values used)
//Furthermore, values clamped beetween -3 and 3

var spei= ee.ImageCollection("GRIDMET/DROUGHT"); //SPEI dataset

var SPEI_monthly = ee.ImageCollection.fromImages(
    years.map(function (y) {
        return spei_m.map(function(m) {
            var vi = spei       .filter(ee.Filter.calendarRange(y, y, 'year'))
                                .filter(ee.Filter.calendarRange(m, m, 'month'))
                                .mean()
                                .clamp(-3,3);
            return vi.set('year', y)
                     .set('month', m)
                     .set('system:time_start', ee.Date.fromYMD(y, m, 1));
        });
    }).flatten()
);

//set SPEI lag
var speiSet = SPEI_monthly.map(function(image) {
     var yearlessone = ee.Number(image.get('year')).add(yearlag);
     var month = image.get('month');
     var dateimage =ee.Date.fromYMD(yearlessone,month,1);
     return image.set('date', dateimage);
  });

//SPEI datasets used
var spei1m= speiSet.select('spei30d');
var spei3m=speiSet.select('spei90d');
var spei6m=speiSet.select('spei180d');
var spei9m=speiSet.select('spei270d');
var spei12m=speiSet.select('spei1y');
var spei24m=speiSet.select('spei2y');

//------------------------------------------------------------------------//
//                       Correlation NG and SPEI  	                      //
//------------------------------------------------------------------------//

//Join the NG and SPEI images
var yearfilter = ee.Filter.equals({
    leftField: 'date',
    rightField: 'date',
});

var yearlink = ee.Join.saveFirst({
    matchKey: 'match',
});


var biomassSet = biomasscorrected.map(function(image) {
  var y =ee.Number.parse(image.get('system:index'));
    return image.set('date', ee.Date.fromYMD(y,speim,01));
  });
  
//A total of 6 images were created joining SPEI_x months with NG
var biomass_spei1m = ee.ImageCollection(yearlink.apply(biomassSet.select('b1'),
        spei1m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });
var biomass_spei3m = ee.ImageCollection(yearlink.apply(biomassSet.select('b1'),
        spei3m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });
var biomass_spei6m = ee.ImageCollection(yearlink.apply(biomassSet.select('b1'),
        spei6m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });
var biomass_spei9m = ee.ImageCollection(yearlink.apply(biomassSet.select('b1'),
        spei9m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });
var biomass_spei12m = ee.ImageCollection(yearlink.apply(biomassSet.select('b1'),
        spei12m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });

var biomass_spei24m = ee.ImageCollection(yearlink.apply(biomassSet.select('b1'),
        spei24m,yearfilter))
        .map(function(image) {
            return image.addBands(image.get('match'));
        });

																 
///////////////////////////COMPUTE PEARSON CORRELATION//////////////////////
var corrmap1m = biomass_spei1m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap3m = biomass_spei3m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap6m = biomass_spei6m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap9m = biomass_spei9m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap12m = biomass_spei12m.reduce(ee.Reducer.pearsonsCorrelation());
var corrmap24m = biomass_spei24m.reduce(ee.Reducer.pearsonsCorrelation());

///////////////////////////COMPUTE KENDALL CORRELATION//////////////////////
/*
var corrmap1m = biomass_spei1m.reduce(ee.Reducer.kendallsCorrelation(2));
var corrmap3m = biomass_spei3m.reduce(ee.Reducer.kendallsCorrelation(2));
var corrmap6m = biomass_spei6m.reduce(ee.Reducer.kendallsCorrelation(2));
var corrmap9m = biomass_spei9m.reduce(ee.Reducer.kendallsCorrelation(2));
var corrmap12m = biomass_spei12m.reduce(ee.Reducer.kendallsCorrelation(2));
var corrmap24m = biomass_spei24m.reduce(ee.Reducer.kendallsCorrelation(2));		
*/
//------------------------------------------------------------------------//
//                      Export the images created  	                      //
//------------------------------------------------------------------------//

Export.image.toAsset({
  image: corrmap1m,
  description: '',
  assetId: 'users/felipef93/P_NG2Y/',
  scale: 2500,
//   region: roi
});

Export.image.toAsset({
  image: corrmap3m,
  description: '',
  assetId: 'users/felipef93/P_NG2Y/',
  scale: 2500,
//   region: roi
});

Export.image.toAsset({
  image: corrmap6m,
  description: '',
  assetId: 'users/felipef93/P_NG2Y/',
  scale: 2500,
//   region: roi
});

Export.image.toAsset({
  image: corrmap9m,
  description: '',
  assetId: 'users/felipef93/P_NG2Y/',
  scale: 2500,
//   region: roi
});

Export.image.toAsset({
  image: corrmap12m,
  description: '',
  assetId: 'users/felipef93/P_NG2Y/',
  scale: 2500,
//   region: roi
});

Export.image.toAsset({
  image: corrmap24m,
  description: '',
  assetId: 'users/felipef93/P_NG2Y/',
  scale: 2500,
//   region: roi
});

