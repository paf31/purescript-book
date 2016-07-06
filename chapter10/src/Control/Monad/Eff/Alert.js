"use strict";

exports.alert = function(msg) {
    return function() {
        window.alert(msg);
        return {};
    };
};
