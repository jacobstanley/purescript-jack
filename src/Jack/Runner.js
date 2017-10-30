"use strict";

exports.findProperties = function (module) {
  return function (tuple) {
    return function () {
      var properties = {};
      var badPropertyNames = [];
      var exports = require(module);

      for (var name in exports) {
        if (name.startsWith("prop_")) {
          var property = exports[name];

          if (typeof property !== "object") {
            // not an object, so not a property test
            badPropertyNames.push(name);
            continue;
          }

          if (!property.hasOwnProperty("Property :: Gen Result")) {
            // 'Property :: Gen Result' field was missing, so not a property test
            continue;
          }

          properties[name] = property;
        }
      }

      return tuple(badPropertyNames)(properties);
    };
  };
};

exports.exit = function (code) {
  return function () {
    process.exit(code);
  };
};
