const kxclib = require('../_output/node_modules/kxclib.melange/kxclib_melange');

const JsonExt = kxclib.Json_ext;

test('it loads', () => {
  expect(kxclib).toBeDefined();
  expect(JsonExt).toBeDefined();
});

test('Json_ext with samples', () => {
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
    { value: [ true, "hello?" ],
      expectedJv: { NAME: 'arr', VAL: { hd: { NAME: 'bool', VAL: true }, tl: { hd: { NAME: 'str', VAL: "hello?"}, tl: 0 }} } },
  ];
  
  for (const { value, expectedJv } of samples) {
    const jv = JsonExt.of_xjv(value);
    const xjv = JsonExt.to_xjv(jv);
    expect(xjv).toStrictEqual(value);
    
    expect(jv).toStrictEqual(expectedJv);
    
    const str = JsonExt.to_json_string(jv);
    const jv_of_str = JsonExt.of_json_string_opt(str);
    expect(jv_of_str).toStrictEqual(jv);
  }
});

test('failure of Json_ext.of_json_string_opt', () => {
  expect(JsonExt.of_json_string_opt('{')).toBeUndefined();
  expect(JsonExt.of_json_string_opt('{age: 12}')).toBeUndefined(); // violating JSON spec as field name not properly quoted
  expect(JsonExt.of_json_string_opt("{'age': 12}")).toBeUndefined(); // violating JSON spec as field name not properly quoted
});
