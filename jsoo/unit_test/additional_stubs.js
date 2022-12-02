//Provides: unix_gethostname
//Requires: caml_string_of_jsstring
function unix_gethostname() {
  return caml_string_of_jsstring("js");
}
