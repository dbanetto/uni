var express = require('express');
var request = require('request');
var router = express.Router();
var Url = require('url').URL;

/* GET home page. */
router.get('/', function(req, res, next) {

  var authed = req.isAuthenticated();

  console.log(req.user);

  res.render('index', { authed: authed, user: req.user, count: 0 });
});

let urls = [
  "https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_128",
  "https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_256",
  "https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_512",
  "https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_1024"
];

router.post('/compute', function(req, res, next) {
  // console.log(req.user);
  console.log(req.body);

  if (!req.isAuthenticated()) {
    res.json({success: false});
    return;
  }

  var size = parseInt(req.body.size);
  console.log(size);
  var url = new Url(urls[size]);
  console.log(url);

  url.searchParams.append('max', req.body.max);
  url.searchParams.append('loops', req.body.loops);

  var options = {
    url:  url.toString(),
    headers: {
      'x-api-key': 'dvgZcZZc8o6PPECyldSMY3GMYGr6CXX45QkswQCs'
    }
  };

  request.get(options, (err, reponse, body) => {
    console.log("IN get");
    console.log(body);
    res.send(body);
  });

  // res.json({success: true});

});

module.exports = router;
