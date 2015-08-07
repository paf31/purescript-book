"use strict";

// module Control.Monad.Eff.Alert

exports.alert = function(msg) {
    return function() {
        window.alert(msg);
        return {};
    };
};
