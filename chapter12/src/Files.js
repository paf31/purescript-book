"use strict";

exports.readFileImpl = function(path, onSuccess, onFailure) {
    return function() {
        require('fs').readFile(path, {
            encoding: 'utf-8'
        }, function(error, data) {
            if (error) {
                onFailure(error.code)();
            } else {
                onSuccess(data)();
            }
        });
    };
};

exports.writeFileImpl = function(path, data, onSuccess, onFailure) {
    return function() {
        require('fs').writeFile(path, data, {
            encoding: 'utf-8'
        }, function(error) {
            if (error) {
                onFailure(error.code)();
            } else {
                onSuccess();
            }
        });
    };
};
