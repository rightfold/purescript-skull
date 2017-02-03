'use strict';

var Control_Monad_Aff = require('../Control.Monad.Aff');

exports.sleep = function(msec) {
  return function(onSuccess, onError) {
    setTimeout(function() {
      onSuccess({});
    }, msec);
    return Control_Monad_Aff.nonCanceler;
  };
};
