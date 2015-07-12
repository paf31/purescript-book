"use strict";

// module Control.Monad.Eff.DOM

exports.body = function() {
    return document.body;
};

exports.createElement = function(name) {
    return function() {
        return document.createElement(name);
    };
};

exports.querySelectorImpl = function(r, f, s) {
    return function() {
        var result = document.querySelector(s);
        return result ? f(result) : r;
    };
};

exports.appendChild = function(child) {
    return function(node) {
        return function() {
            node.appendChild(child);
            return node;
        };
    };
};

exports.addClass = function(className) {
    return function(node) {
        return function() {
            node.classList.add(className);
            return node;
        };
    };
};

exports.setText = function(text) {
    return function(node) {
        return function() {
            node.textContent = text;
            return node;
        };
    };
};

exports.getValue = function(node) {
    return function() {
        return node.value;
    };
};

exports.setValue = function(value) {
    return function(node) {
        return function() {
            node.value = value;
            return node;
        };
    };
};

exports.setInnerHTML = function(html) {
    return function(node) {
        return function() {
            node.innerHTML = html;
            return node;
        };
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
