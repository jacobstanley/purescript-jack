"use strict";

export function findProperties(module) {
  return function () {
    var properties = {};
    var exports = require(module);

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
}

export function exit(code) {
  return function () {
    process.exit(code);
  };
}
