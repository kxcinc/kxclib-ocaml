const kxclib = require('../_output/node_modules/kxclib.melange/kxclib_melange');

const JsonExt = kxclib.Json_ext;

test('it loads', () => {
  expect(kxclib).toBeDefined();
  expect(JsonExt).toBeDefined();
});

test('Json_ext with samples', () => {
  const samples = [
    { value: null },
    { value: true },
    { value: false },
    { value: 131 },
    { value: 131.338 },
    { value: "hello?" },
    { value: "\x58" },
    { value: "日本語" },
    { value: "\u65E5\u672C\u8A9E" },
    { value: [] },
    { value: {"": {}} },
    { value: [ [] ] },
    { value: [ 1, "(a number)"] },
    { value: { best: [], friend: 23 } },
    { value: [ true, "hello?" ] },
  ];
  
  for (const { value } of samples) {
    const jv = JsonExt.of_xjv(value);
    const xjv = JsonExt.to_xjv(jv);
    expect(xjv).toEqual(value);
    
    const str = JsonExt.to_json_string(jv);
    const jv_of_str = JsonExt.of_json_string_opt(str);
    expect(jv_of_str).toEqual(jv);
  }
});

test('failure of Json_ext.of_json_string_opt', () => {
  expect(JsonExt.of_json_string_opt('{')).toBeUndefined();
  expect(JsonExt.of_json_string_opt('{age: 12}')).toBeUndefined(); // violating JSON spec as field name not properly quoted
  expect(JsonExt.of_json_string_opt("{'age': 12}")).toBeUndefined(); // violating JSON spec as field name not properly quoted
});
