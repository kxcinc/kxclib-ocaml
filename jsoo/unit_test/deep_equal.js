/**
 *
 * @type {(o1: string, o2: string) => boolean}
 */
//Provides: json_string_deep_equal
//Requires: caml_js_to_bool, caml_jsstring_of_string
function json_string_deep_equal(json1, json2) {
  return caml_js_to_bool(
    deepEqual(
      JSON.parse(caml_jsstring_of_string(json1)),
      JSON.parse(caml_jsstring_of_string(json2))
    )
  );

  function deepEqual(o1, o2) {
    if (o1 === null && o2 === null) return true;
    if (typeof o1 !== typeof o2) return false;

    if (typeof o1 == "object") {
      const isArray1 = Array.isArray(o1);
      const isArray2 = Array.isArray(o2);
      if (isArray1 && isArray2) {
        if (o1.length !== o2.length) return false;
        return o1.every((v, i) => deepEqual(v, o2[i]));
      } else if (!isArray1 && !isArray2) {
        const ks1 = Object.keys(o1);
        const ks2 = Object.keys(o2);

        if (
          ks1.length === ks2.length &&
          ks1.every((k) => ks2.includes(k)) &&
          ks2.every((k) => ks1.includes(k))
        ) {
          return ks1.every((k) => deepEqual(o1[k], o2[k]));
        } else {
          return false;
        }
      } else {
        return false;
      }
    } else {
      return o1 === o2;
    }
  }
}
