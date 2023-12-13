const kxclib = require('../_output/node_modules/kxclib-melange.core/kxclib');
const kxclib_js = require('../_output/node_modules/kxclib-melange.js/kxclib_js');
const kxclib_melange_test = require('../_output/melange/tests/kxclib_melange_test');

const JsonExt = kxclib_js.Json_ext;

const Json_ext_test = kxclib_melange_test.Json_ext_test;
const Promise_io_test = kxclib_melange_test.Promise_io_test;

test('it loads', () => {
  expect(kxclib).toBeDefined();
  expect(JsonExt).toBeDefined();
  
  expect(kxclib_melange_test).toBeDefined();
  expect(Json_ext_test).toBeDefined();
  expect(Promise_io_test).toBeDefined();
});

const samples = [
  { value: null,
    expectedJv: "null" },
  { value: true,
    expectedJv: { NAME: 'bool', VAL: true } },
  { value: false,
    expectedJv: { NAME: 'bool', VAL: false } },
  { value: 131,
    expectedJv: { NAME: 'num', VAL: 131 } },
  { value: 131.338,
    expectedJv: { NAME: 'num', VAL: 131.338 } },
  { value: "hello?",
    expectedJv: { NAME: 'str', VAL: "hello?" } },
  { value: "\x58",
    expectedJv: { NAME: 'str', VAL: "\x58" } },
  { value: "日本語",
    expectedJv: { NAME: 'str', VAL: "日本語" } },
  { value: "\u65E5\u672C\u8A9E",
    expectedJv: { NAME: 'str', VAL: "\u65E5\u672C\u8A9E" } },
  { value: [],
    expectedJv: { NAME: 'arr', VAL: 0 } },
  { value: {"": {}},
    expectedJv: { NAME: 'obj', VAL: { hd: [ "", { NAME: "obj", VAL: 0 } ], tl: 0 } } },
  { value: [ [] ],
    expectedJv: { NAME: 'arr', VAL: { hd: { NAME: 'arr', VAL: 0 }, tl: 0} } },
  { value: [ 1, "(a number)"],
    expectedJv: { NAME: 'arr', VAL: { hd: { NAME: 'num', VAL: 1 }, tl: { hd: { NAME: 'str', VAL: "(a number)"}, tl: 0 }} } },
  { value: { best: [], friend: 23 },
    expectedJv: { NAME: 'obj', VAL: { hd: [ "best", { NAME: 'arr', VAL: 0 } ], tl: { hd: [ "friend", { NAME: 'num', VAL: 23 }], tl: 0 }} } },
  { value: { best: undefined, friend: 23 }, normalizedValue: {friend: 23},
    expectedJv: { NAME: 'obj', VAL: { hd: [ "friend", { NAME: 'num', VAL: 23 }], tl: 0 } } },
  { value: [ true, "hello?" ],
    expectedJv: { NAME: 'arr', VAL: { hd: { NAME: 'bool', VAL: true }, tl: { hd: { NAME: 'str', VAL: "hello?"}, tl: 0 }} } },
];

test('Json_ext with samples', () => {
  for (const { value, normalizedValue, expectedJv } of samples) {
    const jv = JsonExt.of_xjv(value);
    expect(jv).toStrictEqual(expectedJv);

    const xjv = JsonExt.to_xjv(jv);
    expect(xjv).toStrictEqual(normalizedValue || value);

    const str = JsonExt.to_json_string(jv);
    const jv_of_str = JsonExt.of_json_string_opt(str);
    expect(jv_of_str).toStrictEqual(jv);
  }
});

test('Json_ext with samples (ported', () => {
  Json_ext_test.test_with_samples();
});

test('failure of Json_ext.of_json_string_opt', () => {
  expect(JsonExt.of_json_string_opt('{')).toBeUndefined();
  expect(JsonExt.of_json_string_opt('{age: 12}')).toBeUndefined(); // violating JSON spec as field name not properly quoted
  expect(JsonExt.of_json_string_opt("{'age': 12}")).toBeUndefined(); // violating JSON spec as field name not properly quoted
});

test('failure of Json_ext.of_json_string_opt (ported)', () => {
  Json_ext_test.test_of_json_string_opt_failure();
});

test('successes of Json_ext.of_json_string_opt & Json_ext.to_json_string', () => {
  for (const { value, normalizedValue } of samples) {
    const json = JSON.stringify(value);
    const parsed = JsonExt.of_json_string_opt(json);
    expect(JsonExt.to_xjv(parsed)).toStrictEqual(normalizedValue || value);

    const jv = JsonExt.of_xjv(value);
    const json2 = JsonExt.to_json_string(jv);
    const parsed2 = JSON.parse(json2);
    expect(parsed2).toStrictEqual(normalizedValue || value);
  }
});

test('successes of Json_ext.of_json_string_opt & Json_ext.to_json_string (ported)', () => {
  Json_ext_test.test_string_success();
});

test('Promise_io (ported)', async () => {
  for (const test of Promise_io_test.tests) {
    await test();
  }
});
