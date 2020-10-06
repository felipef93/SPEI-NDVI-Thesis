/* 
This script adds metadata information in the form of metadata to an image collection 
Herein the imagecollection contains R(band1) and P(band2). 
Each image corresponds to a scenario of varied NDVI lag, NDVI range, SPEI month and SPEI range
The function will create a band for each variable: NDVIL(B3),NDVIR(B4),SPEIM(B5),SPEIR(B6)
*/

//------------------------------------------------------------------------//
//                               Datainput                                //
//------------------------------------------------------------------------//
var corr = ee.ImageCollection("users/felipef93/CP3");//Image collection containing R and P values of several scenarios
var metadata =ee.FeatureCollection("users/felipef93/P3metadata"); // csv file containing a description of the scenarios

//------------------------------------------------------------------------//
//                               Processing                               //
//------------------------------------------------------------------------//
// Function to assign the metadata to the image collection
var image_meta = function (image){
  var id = image.id();
  var corresponding = metadata.filter(ee.Filter.eq('Sim',id));
  var listed =ee.List(corresponding.toList(9));
  var feature  =ee.Feature(listed.get(0));
  return image.set({NDVIR: feature.get('NDVI range'),NDVIM:feature.get('NDVI Month'),SPEIL:feature.get('SPEI LAG'),SPEIR:feature.get('SPEI range')});
};

//Function to mask locations with spurious correlations
var newimgs = corr.map(image_meta);
var maskspurious =function(image){
  var mask = image.select('p-value').lte(0.05);
  return image.updateMask(mask);
};

var newimgs = newimgs.map(maskspurious);

//Creation of exported rasters. Here exporting the best and worst correlations according to SPEI range
var spei1m =newimgs.filter(ee.Filter.eq('SPEIL',0));
var spei3m =newimgs.filter(ee.Filter.eq('SPEIL',1));
var spei6m =newimgs.filter(ee.Filter.eq('SPEIL',2));
var spei9m =newimgs.filter(ee.Filter.eq('SPEIL',3));
var spei12m =newimgs.filter(ee.Filter.eq('SPEIL',6));
var spei24m =newimgs.filter(ee.Filter.eq('SPEIL',12));

var max1m = spei1m.reduce(ee.Reducer.max(2));
var max1m=max1m.updateMask(max1m.select(0).gte(0));
var min1m = spei1m.reduce(ee.Reducer.min(2));
var min1m=min1m.updateMask(min1m.select(0).lte(0));

var max3m = spei3m.reduce(ee.Reducer.max(2));
var max3m=max3m.updateMask(max3m.select(0).gte(0));
var min3m = spei3m.reduce(ee.Reducer.min(2));
var min3m=min3m.updateMask(min3m.select(0).lte(0));

var max6m = spei6m.reduce(ee.Reducer.max(2));
var max6m=max6m.updateMask(max6m.select(0).gte(0));
var min6m = spei6m.reduce(ee.Reducer.min(2));
var min6m=min6m.updateMask(min6m.select(0).lte(0));

var max9m = spei9m.reduce(ee.Reducer.max(2));
var max9m=max9m.updateMask(max9m.select(0).gte(0));
var min9m = spei9m.reduce(ee.Reducer.min(2));
var min9m=min9m.updateMask(min9m.select(0).lte(0));

var max12m = spei12m.reduce(ee.Reducer.max(2));
var max12m=max12m.updateMask(max12m.select(0).gte(0));
var min12m = spei12m.reduce(ee.Reducer.min(2));
var min12m=min12m.updateMask(min12m.select(0).lte(0));

var max24m = spei24m.reduce(ee.Reducer.max(2));
var max24m=max24m.updateMask(max24m.select(0).gte(0));
var min24m = spei24m.reduce(ee.Reducer.min(2));
var min24m=min24m.updateMask(min24m.select(0).lte(0));

//------------------------------------------------------------------------//
//                               	Export                                //
//------------------------------------------------------------------------//
Export.image.toDrive({
  image: max1m,
  description: 'SPEIL0_max',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: min1m,
  description: 'SPEIL0_min',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max3m,
  description: 'SPEIL1_max',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: min3m,
  description: 'SPEIL1_min',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max6m,
  description: 'SPEIL2_max',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: min6m,
  description: 'SPEIL2_min',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max9m,
  description: 'SPEIL3_max',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: min9m,
  description: 'SPEIL3_min',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max12m,
  description: 'SPEIL6_max',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: min12m,
  description: 'SPEIL6_min',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max24m,
  description: 'SPEIL12_max',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: min24m,
  description: 'SPEIL12_min',
  scale: 2500,
//   region: roi
});
