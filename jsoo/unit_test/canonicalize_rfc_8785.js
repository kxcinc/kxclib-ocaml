//Provides: _canonicalize_rfc_8785_TextEncoder
var _canonicalize_rfc_8785_TextEncoder = new TextEncoder()

/**
 * @type {(jsonString: string) => unknown}
 * https://www.rfc-editor.org/rfc/rfc8785.html
*/
//Provides: canonicalize_rfc_8785
//Requires: _canonicalize_rfc_8785_TextEncoder
//Requires: caml_bytes_of_array
function canonicalize_rfc_8785(jsonString) {
  var buffer = "";
  serialize(JSON.parse(jsonString));
  return caml_bytes_of_array(_canonicalize_rfc_8785_TextEncoder.encode(buffer));

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
