import outdent from "outdent";
import type * as rpc from "vscode-jsonrpc/node";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as LanguageServer from "../src/LanguageServer";

describe("textDocument/diagnostics", () => {
  let languageServer: rpc.MessageConnection;

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          "file:///test.ml",
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("has related diagnostics", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "This comment contains an unterminated string literal",
      "range": {
        "end": {
          "character": 2,
          "line": 0,
        },
        "start": {
          "character": 0,
          "line": 0,
        },
      },
      "relatedInformation": [
        {
          "location": {
            "range": {
              "end": {
                "character": 4,
                "line": 0,
              },
              "start": {
                "character": 3,
                "line": 0,
              },
            },
            "uri": "file:///test.ml",
          },
          "message": "String literal begins here",
        },
      ],
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );
    openDocument(outdent`
(* " *)
    `);
    await receivedDiganostics;
  });

  it("unused values have diagnostic tags", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Warning 26: unused variable x.",
      "range": {
        "end": {
          "character": 7,
          "line": 1,
        },
        "start": {
          "character": 6,
          "line": 1,
        },
      },
      "severity": 2,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );
    openDocument(outdent`
      let () =
        let x = 123 in
        ()
    `);
    await receivedDiganostics;
  });

  it("deprecated values have diganostic tags", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Alert deprecated: X.x
do not use",
      "range": {
        "end": {
          "character": 19,
          "line": 6,
        },
        "start": {
          "character": 16,
          "line": 6,
        },
      },
      "severity": 2,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );
    openDocument(outdent`
      module X : sig
        val x : unit
        [@@ocaml.deprecated "do not use"]
      end = struct
        let x = ()
      end
      let () = ignore X.x
    `);
    await receivedDiganostics;
  });

  it("related diagnostics for mismatched signatures", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "Signature mismatch:
Modules do not match:
  sig val x : int end
is not included in
  sig val x : unit end
Values do not match: val x : int is not included in val x : unit
The type int is not compatible with the type unit",
      "range": {
        "end": {
          "character": 3,
          "line": 4,
        },
        "start": {
          "character": 6,
          "line": 2,
        },
      },
      "relatedInformation": [
        {
          "location": {
            "range": {
              "end": {
                "character": 14,
                "line": 2,
              },
              "start": {
                "character": 2,
                "line": 2,
              },
            },
            "uri": "file:///test.ml",
          },
          "message": "Expected declaration",
        },
        {
          "location": {
            "range": {
              "end": {
                "character": 7,
                "line": 4,
              },
              "start": {
                "character": 6,
                "line": 4,
              },
            },
            "uri": "file:///test.ml",
          },
          "message": "Actual declaration",
        },
      ],
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );
    openDocument(outdent`
      module X : sig
        val x : unit
      end = struct
        let x = 123
      end
    `);
    await receivedDiganostics;
  });

  it("no diagnostics for valid files", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );

    openDocument(outdent`
      let num = 42
    `);

    await receivedDiganostics;
  });

  it("should have diagnostics for a hole only", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "code": "hole",
      "message": "This typed hole should be replaced with an expression of type int",
      "range": {
        "end": {
          "character": 15,
          "line": 0,
        },
        "start": {
          "character": 14,
          "line": 0,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );

    openDocument(outdent`
      let a : int = _
    `);

    await receivedDiganostics;
  });

  it("should have diagnostics for holes only", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "code": "hole",
      "message": "This typed hole should be replaced with an expression of type int",
      "range": {
        "end": {
          "character": 16,
          "line": 0,
        },
        "start": {
          "character": 15,
          "line": 0,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
    {
      "message": "Warning 26: unused variable b.",
      "range": {
        "end": {
          "character": 5,
          "line": 1,
        },
        "start": {
          "character": 4,
          "line": 1,
        },
      },
      "severity": 2,
      "source": "ocamllsp",
    },
    {
      "code": "hole",
      "message": "This typed hole should be replaced with an expression of type string",
      "range": {
        "end": {
          "character": 44,
          "line": 1,
        },
        "start": {
          "character": 43,
          "line": 1,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
    {
      "code": "hole",
      "message": "This typed hole should be replaced with an expression of type string",
      "range": {
        "end": {
          "character": 58,
          "line": 1,
        },
        "start": {
          "character": 57,
          "line": 1,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );

    openDocument(outdent`
      let _a : int = _ in
      let b : string = match Some 1 with None -> _ | Some _ -> _ in
      ()
    `);

    await receivedDiganostics;
  });

  it("different diagnostics, including holes, sorted by range", async () => {
    const receivedDiganostics = new Promise((resolve, _reject) =>
      languageServer.onNotification((method, params) => {
        expect(method).toMatchInlineSnapshot(
          `"textDocument/publishDiagnostics"`,
        );
        expect(params).toMatchInlineSnapshot(`
{
  "diagnostics": [
    {
      "message": "The constructor () has type unit but an expression was expected of type int",
      "range": {
        "end": {
          "character": 42,
          "line": 1,
        },
        "start": {
          "character": 40,
          "line": 1,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
    {
      "code": "hole",
      "message": "This typed hole should be replaced with an expression of type 'a",
      "range": {
        "end": {
          "character": 12,
          "line": 3,
        },
        "start": {
          "character": 11,
          "line": 3,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
    {
      "code": "hole",
      "message": "This typed hole should be replaced with an expression of type 'a",
      "range": {
        "end": {
          "character": 12,
          "line": 4,
        },
        "start": {
          "character": 11,
          "line": 4,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
    {
      "message": "This constant has type string but an expression was expected of type int",
      "range": {
        "end": {
          "character": 9,
          "line": 5,
        },
        "start": {
          "character": 6,
          "line": 5,
        },
      },
      "severity": 1,
      "source": "ocamllsp",
    },
  ],
  "uri": "file:///test.ml",
}
`);
        resolve(null);
      }),
    );

    openDocument(outdent`
      let _u =
        let _i = List.map (fun i -> i) [1; 2; ()] in
        let b = 234 in
        let _k = _ in
        let _c = _ in
        b + "a"
    `);

    await receivedDiganostics;
  });
});
