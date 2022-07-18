const kxclib = require('./kxclib.bs')

test('it loads', () => {
  expect(kxclib).toBeDefined();
});

test('Json_ext correctness', () => {
  let JsonExt = kxclib.Kxclib_comp.Json_ext;
  let Samples = JsonExt.TestSamples;

  let expected_sample_obj01 = {
    name: "James Smith",
    age: 12,
  };
  let expected_sample_obj02 = {
    name: "James Smith",
    age: 12,
    spouse: null,
    parents: ["Amy Smith", "Bob Smith"]
  };

  // check Json_ext.to_xjv
  expect(Samples.xjv_conv_obj00).toEqual({});
  expect(Samples.xjv_conv_obj01).toEqual(expected_sample_obj01);
  expect(Samples.xjv_conv_obj02).toEqual(expected_sample_obj02);

  // check Json_ext.of_xjv
  expect(JsonExt.of_xjv({})).toEqual(Samples.obj00);
  expect(JsonExt.of_xjv(expected_sample_obj01)).toEqual(Samples.obj01);
  expect(JsonExt.of_xjv(expected_sample_obj02)).toEqual(Samples.obj02);

  // check json_of_*
  expect(JsonExt.json_of_jv(Samples.obj00)).toEqual("{}");
  expect(JSON.parse(JsonExt.json_of_jv(Samples.obj01))).toEqual(expected_sample_obj01);
  expect(JSON.parse(JsonExt.json_of_jv(Samples.obj02))).toEqual(expected_sample_obj02);

  expect(JsonExt.json_of_xjv(Samples.xjv_conv_obj00)).toEqual("{}");
  expect(JSON.parse(JsonExt.json_of_xjv(Samples.xjv_conv_obj01))).toEqual(expected_sample_obj01);
  expect(JSON.parse(JsonExt.json_of_xjv(Samples.xjv_conv_obj02))).toEqual(expected_sample_obj02);

  // check *_of_json
  expect(JsonExt.jv_of_json_exn(JSON.stringify({}))).toEqual(Samples.obj00);
  expect(JsonExt.jv_of_json_exn(JSON.stringify(expected_sample_obj01))).toEqual(Samples.obj01);
  expect(JsonExt.jv_of_json_exn(JSON.stringify(expected_sample_obj02))).toEqual(Samples.obj02);

  expect(JsonExt.jv_of_json('{')).toBeUndefined();
  expect(JsonExt.jv_of_json('{age: 12}')).toBeUndefined(); // violating JSON spec as field name not properly quoted
  expect(JsonExt.jv_of_json("{'age': 12}")).toBeUndefined(); // violating JSON spec as field name not properly quoted
});

test('Base64 correctness', () => {
  let Base64 = kxclib.Base64;
  let InterOp = kxclib.Kxclib_comp_re.JsInterop;
  let { encode: _encode, decode: _decode } = Base64;
  let toUint8Array = buf => new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength);
  let bytesHex = hex_str => InterOp.bytes_of_uint8array(toUint8Array(Buffer.from(hex_str, 'hex')));
  let bytesStr = hex_str => InterOp.bytes_of_uint8array(toUint8Array(Buffer.from(hex_str, 'utf8')));
  let encode = hex_str => _encode(undefined, undefined, bytesStr(hex_str));
  let decode = base64str => InterOp.uint8array_of_bytes(_decode(undefined, undefined, base64str));

  let expectUint8ArrayEqual = (actual, expected) => expect(actual.toString()).toEqual(expected.toString());

  expect(encode("")).toEqual("");
  expect(encode("f")).toEqual("Zg==");
  expect(encode("fo")).toEqual("Zm8=");
  expect(encode("foobar")).toEqual("Zm9vYmFy");
  expect(encode("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna."))
    .toEqual("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYS4=");

  expectUint8ArrayEqual(decode("/+7dzLuqmQ=="), bytesHex("ffeeddccbbaa99"));
  expectUint8ArrayEqual(decode("AAA="), bytesHex("0000"));
});
