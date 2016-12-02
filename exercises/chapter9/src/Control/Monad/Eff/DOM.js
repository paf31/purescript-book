"use strict";

exports.querySelectorImpl = function(r, f, s) {
    return function() {
        var result = document.querySelector(s);
        return result ? f(result) : r;
    };
};

exports.addEventListener = function(name) {
    return function(handler) {
        return function(node) {
            return function() {
                node.addEventListener(name, function(e) {
                    handler();
                    e.preventDefault();
                });
            };
        };
    };
};
