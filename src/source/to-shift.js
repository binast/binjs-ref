'use strict';

function removeFunctionContents(obj) {
    obj.type = obj.type.replace(/^(?:Eager|Lazy)/, '');

    delete obj.isAsync;
    delete obj.scope;
    delete obj.contents_skip;
    delete obj.length;

    let { params, body } = obj.contents;
    delete obj.contents;

    obj.params = params;

    if (obj.type === 'ArrowExpressionWithExpression') {
        obj.type = 'ArrowExpression';
        obj.body = body;
    } else {
        if (obj.type === 'ArrowExpressionWithFunctionBody') {
            obj.type = 'ArrowExpression';
        }
        obj.body = {
            type: 'FunctionBody',
            directives: obj.directives,
            statements: body
        };
    }
}

function convertObject(key, value) {}
