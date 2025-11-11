/**
 * @type {(object: any) => string}
 * https://tex2e.github.io/rfc-translater/html/rfc8785.html
*/
//Provides: canonicalize_rfc_8785
//Requires: caml_string_of_jsstring
function canonicalize_rfc_8785(object) {
  var buffer = "";
  serialize(object);
  return caml_string_of_jsstring(buffer);

  function serialize(object) {
    if (
      object === null ||
      typeof object !== "object" ||
      object.toJSON != null
    ) {
      /////////////////////////////////////////////////
      // Primitive type or toJSON, use "JSON"        //
      /////////////////////////////////////////////////
      buffer += JSON.stringify(object);
    } else if (Array.isArray(object)) {
      /////////////////////////////////////////////////
      // Array - Maintain element order              //
      /////////////////////////////////////////////////
      buffer += "[";
      let next = false;
      object.forEach((element) => {
        if (next) {
          buffer += ",";
        }
        next = true;
        /////////////////////////////////////////
        // Array element - Recursive expansion //
        /////////////////////////////////////////
        serialize(element);
      });
      buffer += "]";
    } else {
      /////////////////////////////////////////////////
      // Object - Sort properties before serializing //
      /////////////////////////////////////////////////
      buffer += "{";
      let next = false;
      Object.keys(object)
        .sort()
        .forEach((property) => {
          if (next) {
            buffer += ",";
          }
          next = true;
          /////////////////////////////////////////////
          // Property names are strings, use "JSON"  //
          /////////////////////////////////////////////
          buffer += JSON.stringify(property);
          buffer += ":";
          //////////////////////////////////////////
          // Property value - Recursive expansion //
          //////////////////////////////////////////
          serialize(object[property]);
        });
      buffer += "}";
    }
  }
};
