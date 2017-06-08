#![allow(unused_imports)]
extern crate binjs;
extern crate glob;
extern crate serde_json;

use serde_json::Value as JSON;

use std::fs::File;
use std::io::Read;
use std::path::*;

#[test]
#[cfg(feature = "spidermonkey")]
fn test_spidermonkey() {
    println!("Setting up SpiderMonkey");
    let path_to_spidermonkey = std::env::var("BINJS_SPIDERMONKEY")
        .expect("To run this test, please set BINJS_SPIDERMONKEY to the path of the spidermonkey `js` binary.");
    let spidermonkey = binjs::source::SpiderMonkey::with_path(path_to_spidermonkey);

    println!("Setting up whitelist");
    // The following files have different behavior in Esprima and SpiderMonkey.
    let whitelist : Vec<_> = [
        "ES6/binding-pattern/array-pattern/for-let-let.tree.json",
        "ES6/binding-pattern/object-pattern/for-let-let.tree.json",
        "ES6/export-declaration/export-const-number.tree.json",
        "ES6/export-declaration/export-default-array.tree.json",
        "ES6/export-declaration/export-default-assignment.module.tree.json",
        "ES6/export-declaration/export-default-class.tree.json",
        "ES6/export-declaration/export-default-expression.tree.json",
        "ES6/export-declaration/export-default-function.tree.json",
        "ES6/export-declaration/export-default-named-class.tree.json",
        "ES6/export-declaration/export-default-named-function.tree.json",
        "ES6/export-declaration/export-default-number.tree.json",
        "ES6/export-declaration/export-default-object.tree.json",
        "ES6/export-declaration/export-default-value.tree.json",
        "ES6/export-declaration/export-from-batch.tree.json",
        "ES6/export-declaration/export-from-default.tree.json",
        "ES6/export-declaration/export-from-named-as-default.tree.json",
        "ES6/export-declaration/export-from-named-as-specifier.tree.json",
        "ES6/export-declaration/export-from-named-as-specifiers.tree.json",
        "ES6/export-declaration/export-from-specifier.tree.json",
        "ES6/export-declaration/export-from-specifiers.tree.json",
        "ES6/export-declaration/export-function-declaration.tree.json",
        "ES6/export-declaration/export-function.tree.json",
        "ES6/export-declaration/export-let-number.tree.json",
        "ES6/export-declaration/export-named-as-default.tree.json",
        "ES6/export-declaration/export-named-as-specifier.tree.json",
        "ES6/export-declaration/export-named-as-specifiers.tree.json",
        "ES6/export-declaration/export-named-empty.tree.json",
        "ES6/export-declaration/export-named-keyword-as-specifier.tree.json",
        "ES6/export-declaration/export-named-keyword-specifier.tree.json",
        "ES6/export-declaration/export-named-specifier.tree.json",
        "ES6/export-declaration/export-named-specifiers-comma.tree.json",
        "ES6/export-declaration/export-named-specifiers.tree.json",
        "ES6/export-declaration/export-var-anonymous-function.tree.json",
        "ES6/export-declaration/export-var-number.tree.json",
        "ES6/export-declaration/export-var.tree.json",
        "ES6/import-declaration/import-default-and-named-specifiers.tree.json",
        "ES6/import-declaration/import-default-and-namespace-specifiers.tree.json",
        "ES6/import-declaration/import-default-as.tree.json",
        "ES6/import-declaration/import-default.tree.json",
        "ES6/import-declaration/import-jquery.tree.json",
        "ES6/import-declaration/import-module.tree.json",
        "ES6/import-declaration/import-named-as-specifier.tree.json",
        "ES6/import-declaration/import-named-as-specifiers.tree.json",
        "ES6/import-declaration/import-named-empty.tree.json",
        "ES6/import-declaration/import-named-specifier.tree.json",
        "ES6/import-declaration/import-named-specifiers-comma.tree.json",
        "ES6/import-declaration/import-named-specifiers.tree.json",
        "ES6/import-declaration/import-namespace-specifier.tree.json",
        "ES6/import-declaration/import-null-as-nil.tree.json",
        "ES6/template-literals/escape-sequences.tree.json",
        "ES6/template-literals/line-terminators.tree.json",
        "ES6/template-literals/literal-escape-sequences.tree.json",
        "ES6/unicode-code-point-escape-sequence/migrated_0000.tree.json",
        "ES6/unicode-code-point-escape-sequence/migrated_0001.tree.json",
        "ES6/unicode-code-point-escape-sequence/migrated_0002.tree.json",
        "ES6/yield/yield-generator-arrow-default.tree.json",
        "ES6/yield/yield-generator-default-parameter.tree.json",
        "JSX/attribute-double-quoted-string.tree.json",
        "JSX/attribute-element.tree.json",
        "JSX/attribute-empty-entity1.tree.json",
        "JSX/attribute-empty-entity2.tree.json",
        "JSX/attribute-entity-decimal.tree.json",
        "JSX/attribute-entity-hex.tree.json",
        "JSX/attribute-entity.tree.json",
        "JSX/attribute-expression.tree.json",
        "JSX/attribute-illegal-short-entity.tree.json",
        "JSX/attribute-invalid-entity.tree.json",
        "JSX/attribute-multi-entities.tree.json",
        "JSX/attribute-non-hex-entity.tree.json",
        "JSX/attribute-non-numeric-entity.tree.json",
        "JSX/attribute-null-value.tree.json",
        "JSX/attribute-primary.tree.json",
        "JSX/attribute-single-quoted-string.tree.json",
        "JSX/attribute-spread.tree.json",
        "JSX/attribute-unknown-entity.tree.json",
        "JSX/attribute-unterminated-entity.tree.json",
        "JSX/attribute-x-entity.tree.json",
        "JSX/container-numeric-literal.tree.json",
        "JSX/container-object-expression.tree.json",
        "JSX/container-series.tree.json",
        "JSX/empty-child-comment.tree.json",
        "JSX/empty-expression-container.tree.json",
        "JSX/inside-group-expression.tree.json",
        "JSX/long-member-pair.tree.json",
        "JSX/long-member.tree.json",
        "JSX/multi-attributes.tree.json",
        "JSX/multiline-crlf-text.tree.json",
        "JSX/multiline-text.tree.json",
        "JSX/nested-elements.tree.json",
        "JSX/null-attribute-value.tree.json",
        "JSX/simple-deeply-nested-pair.tree.json",
        "JSX/simple-expression-container.tree.json",
        "JSX/simple-member-pair.tree.json",
        "JSX/simple-member.tree.json",
        "JSX/simple-namespace-pair.tree.json",
        "JSX/simple-namespace.tree.json",
        "JSX/simple-nested-pair.tree.json",
        "JSX/simple-pair.tree.json",
        "JSX/simple-selfclosing-linefeed.tree.json",
        "JSX/simple-selfclosing-whitespace.tree.json",
        "JSX/simple-selfclosing.tree.json",
        "JSX/simple-text.tree.json",
        "JSX/template-literal.tree.json",
        "JSX/yield-jsx-element.tree.json",
        "es2017/async/arrows/export-async-arrow.module.tree.json",
        "es2017/async/arrows/export-default-async-arrow.module.tree.json",
        "es2017/async/functions/async-if-await.tree.json",
        "es2017/async/functions/async-if.tree.json",
        "es2017/async/functions/export-async-function-declaration-await.module.tree.json",
        "es2017/async/functions/export-async-function-declaration.module.tree.json",
        "es2017/async/functions/export-default-async-function-declaration.module.tree.json",
        "es2017/async/functions/export-default-async-named-function-declaration-await.module.tree.json",
        "es2017/async/functions/export-default-async-named-function-declaration.module.tree.json",
        "es2017/async/regular-identifier/export-identifier-async.module.tree.json",
        "es2017/trailing-commas/trailing-comma-export-function.tree.json",
        "es2018/dynamic-import/await-import.tree.json",
        "es2018/dynamic-import/coexist-import-call-import-declaration.module.tree.json",
        "es2018/dynamic-import/import-call-string.tree.json",
        "es2018/dynamic-import/import-call-template.tree.json",
        "es2018/dynamic-import/import-call-var.tree.json",
        "es2018/dynamic-import/loader-using-import.tree.json",
        "es2018/rest-property/destructuring-mirror.tree.json",
        "es2018/rest-property/function-extension.tree.json",
        "es2018/rest-property/rest-property-object-pattern-arrow.tree.json",
        "es2018/rest-property/shallow-clone.tree.json",
        "es2018/rest-property/simple-rest-properties.tree.json",
        "es2018/spread-property/default-properties.tree.json",
        "es2018/spread-property/multiple-merges.tree.json",
        "es2018/spread-property/object-initializer-getter.tree.json",
        "es2018/spread-property/object-merging.tree.json",
        "es2018/spread-property/properties-overriding.tree.json",
        "es2018/spread-property/shallow-clone.tree.json",
        "es2018/spread-property/spread-getter.tree.json",
        "es2018/spread-property/spread-null-undefined.tree.json",
        "expression/primary/array/migrated_0008.tree.json",
        "expression/primary/array/migrated_0009.tree.json",
        "expression/primary/array/migrated_0010.tree.json",
        "expression/primary/array/migrated_0011.tree.json",
        "expression/primary/array/migrated_0012.tree.json",
        "expression/primary/literal/regular-expression/migrated_0005.tree.json",
        "expression/primary/literal/string/migrated_0002.tree.json",
        "statement/expression/migrated_0002.tree.json",
        "statement/expression/migrated_0003.tree.json",
        "statement/expression/migrated_0004.tree.json",
        "statement/expression/migrated_0005.tree.json",
        "whitespace/migrated_0000.tree.json",
        "whitespace/migrated_0001.tree.json"
    ].iter().cloned().map(Path::new).collect();

    println!("Locating source files");
    let mut path = PathBuf::new();
    for item in &[env!("CARGO_MANIFEST_DIR"), "tests", "data", "esprima", "test", "fixtures"] {
        path.push(item);
    }
    let path = path;
    let glob_format = format!("{}/**/*.tree.json", path.to_str().expect("Could not convert path to string"));

    let mut failures = vec![];
    'test_file: for metadata_path in glob::glob(&glob_format).expect("Could not glob path") {
        if let Ok(path) = metadata_path {
            println!("Analyzing source file metadata {:?}.", path);
            const SUFFIX : &'static str = ".tree.json";

            for white in &whitelist {
                if path.ends_with(white) {
                    println!("Skipping whitelisted file");
                    continue 'test_file;
                }
            }

            let mut metadata_file = File::open(path.clone())
                .expect("Could not open metadata file");
            let mut metadata_source = String::new();
            metadata_file
                .read_to_string(&mut metadata_source)
                .expect("Could not read metadata file");

            let metadata : JSON = serde_json::from_str(&metadata_source)
                .expect("Could not parse metadata");
            let metadata = metadata.as_object().unwrap();

            let path = path.to_str()
                .expect("Could not convert path to string");
            let (prefix, _) = path.split_at(path.len() - SUFFIX.len());

            let source_path = format!("{}.js", prefix);

            println!("Parsing source file {}", source_path);
            let parse_result = spidermonkey.parse_file(&source_path);

            if metadata.get("errors").is_some() {
                println!("Expecting parse failure.");
                // FIXME: TODO
            } else {
                println!("Expecting parse success.");
                if parse_result.is_err() {
                    println!("Parse failed.");
                    failures.push(path.to_owned())
                }
            }
        }
    }

    if failures.len() != 0 {
        panic!("Unexpected failures {:?}", failures);
    }
}