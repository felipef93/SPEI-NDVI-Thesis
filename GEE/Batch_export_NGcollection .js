var NG = ee.ImageCollection('users/felipef93/PNG2Y'); //NG collection
var ecoregion = ee.FeatureCollection("users/felipef93/Ecocal_project");  //Ecoregion filters in california
var metadata =ee.FeatureCollection("users/felipef93/NGmeta2Y2"); // csv file containing a description of the scenarios
var state = ['California'];
var usstate = ee.FeatureCollection("TIGER/2018/States");//us state vector
var stateshape = usstate.filter(ee.Filter.inList('NAME', state));// us state
var roi = stateshape.geometry();// us state//Define region of interest here

//Function to add metadata to imgs
var image_meta = function (image){
  var id = image.id();
  var corresponding = metadata.filter(ee.Filter.eq('Sim num',id));
  var listed =ee.List(corresponding.toList(9));
  var feature  =ee.Feature(listed.get(0));
  return image.set({SPEIR: feature.get('SPEI range'),SPEIM:feature.get('SPEImonth'),SPEIS:feature.get('SPEIS'),SPEIY:feature.get('SPEIY')});
};

var newimgs = NG.map(image_meta); // Function is applied

// Batches were exported in batches of 30 as it was what the programming interface supported
var testimport = newimgs.filterMetadata('system:index',"greater_than",'130').filterMetadata('system:index', "not_greater_than",'162');

var batch = require('users/fitoprincipe/geetools:batch'); //Fitoprincipe has great friendly GEE tools
batch.Download.ImageCollection.toDrive(testimport, 'Test', 
                {scale: 2500,
                 region: roi
                });