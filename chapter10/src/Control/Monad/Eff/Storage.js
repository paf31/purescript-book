"use strict";

exports.setItem = function(key) {
    return function(value) {
        return function() {
            window.localStorage.setItem(key, value);
        };
    };
};

exports.getItem = function(key) {
    return function() {
        return window.localStorage.getItem(key);
    }
};
