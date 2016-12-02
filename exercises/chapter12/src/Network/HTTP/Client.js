"use strict";

exports.getImpl = function(uri, done, fail) {
    return function() {
        require('request')(uri, function(err, _, body) {
            if (err) {
                fail(err)();
            } else {
                done(body)();
            }
        });
    };
};
