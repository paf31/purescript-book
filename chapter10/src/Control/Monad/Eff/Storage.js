"use strict";

// module Control.Monad.Eff.Storage

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