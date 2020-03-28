"use strict";

exports.findProperties = function (module) {
  return function () {
    var properties = {};
    // Using `require(module);` won't find the correct module
    // By appending `../` to its front, it'll find the correct module.
    var exports = require("../" + module);

    for (var name in exports) {
      if (name.startsWith("prop_")) {
        var property = exports[name];

        if (typeof property !== "object") {
          // not an object, so not a property test
          continue;
        }

        if (!property.hasOwnProperty("Property :: Gen Result")) {
          // 'Property :: Gen Result' field was missing, so not a property test
          continue;
        }

        properties[name] = property;
      }
    }

    return properties;
  };
};

exports.exit = function (code) {
  return function () {
    process.exit(code);
  };
};
