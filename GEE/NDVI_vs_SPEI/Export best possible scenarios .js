/* 
This script adds metadata information in the form of bands to an image collection 
Herein the imagecollection contains R(band1) and P(band2). 
Each image corresponds to a scenario of varied NDVI lag, NDVI range, SPEI month and SPEI range
The function will create a band for each variable: NDVIL(B3),NDVIR(B4),SPEIM(B5),SPEIR(B6)
*/

//------------------------------------------------------------------------//
//                               Datainput                                //
//------------------------------------------------------------------------//
var corr = ee.ImageCollection("users/felipef93/CP3");//Image collection containing R and P values of several scenarios
var metadata =ee.FeatureCollection("users/felipef93/P3metadataa"); // csv file containing a description of the scenarios

//------------------------------------------------------------------------//
//                               Processing                               //
//------------------------------------------------------------------------//
// Function to assign the metadata to the image collection
var image_meta = function (image){
  var id = image.id();
  var corresponding = metadata.filter(ee.Filter.eq('Sim',id));
  var listed =ee.List(corresponding.toList(9));
  var feature  =ee.Feature(listed.get(0));
  //Metadata is imported as a feature
  var NDVIM = ee.Feature(image.geometry(),{'val':feature.get('NDVIM')});
  var NDVIR =ee.Feature(image.geometry(),{'val':feature.get('NDVIR')});
  var SPEIL = ee.Feature(image.geometry(),{'val':feature.get('SPEIL')});
  var SPEIR = ee.Feature(image.geometry(),{'val':feature.get('SPEIR')});
  // Metadata transformed to image and then added as a band in corresponding image
  var NDVIMi= ee.FeatureCollection([NDVIM]).reduceToImage(['val'],ee.Reducer.first()).rename('NDVIM');
  var NDVIRi= ee.FeatureCollection([NDVIR]).reduceToImage(['val'],ee.Reducer.first()).rename('NDVIR');
  var SPEILi=ee.FeatureCollection([SPEIL]).reduceToImage(['val'],ee.Reducer.first()).rename('SPEIL');
  var SPEIRi= ee.FeatureCollection([SPEIR]).reduceToImage(['val'],ee.Reducer.first()).rename('SPEIR');
  return image.addBands([NDVIMi,NDVIRi,SPEILi,SPEIRi]);
};

var newimgs = corr.map(image_meta);

//Function to mask locations with spurious correlations
var maskspurious =function(image){
  var mask = image.select('p-value').lte(0.05);
  return image.updateMask(mask);
};
var newimgs = newimgs.map(maskspurious);


//Creation of best possible scenarios Image where max is highest correlations and min lowest
var max = newimgs.reduce(ee.Reducer.max(6)).rename('correlation','p','NDVIM','NDVIR','SPEIL','SPEIR');
var max=max.updateMask(max.select(0).gte(0));
var min = newimgs.reduce(ee.Reducer.min(6)).rename('correlation','p','NDVIM','NDVIR','SPEIL','SPEIR');
var min=min.updateMask(min.select(0).lte(0));

//------------------------------------------------------------------------//
//                               	Export                                //
//------------------------------------------------------------------------//

Export.image.toDrive({
  image: max.select('NDVIM'),
  description: 'posNDVIM',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image:  min.select('NDVIM'),
  description: 'negNDVIM',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max.select('NDVIR'),
  description: 'posNDVIR',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image:  min.select('NDVIR'),
  description: 'negNDVIR',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max.select('SPEIR'),
  description: 'posSPEIR',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image:  min.select('SPEIR'),
  description: 'negSPEIR',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image: max.select('SPEIL'),
  description: 'posSPEIL',
  scale: 2500,
//   region: roi
});

Export.image.toDrive({
  image:  min.select('SPEIL'),
  description: 'negSPEIL',
  scale: 2500,
//   region: roi
});