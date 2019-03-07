'use strict';

function makeEager(obj) {
    obj.type = 'Eager' + obj.type;
}

function dummyDeclaredScope(type) {
    return {
        type,
        declaredNames: [],
        hasDirectEval: false
    };
}

function parameterScopeAndLength(params) {
    let length = 0;
    let isSimpleParameterList = true;

    loop: for (let param of params) {
        switch (param.type) {
            case 'BindingIdentifier':
                length++;
                break;
            case 'ObjectBinding':
            case 'ArrayBinding':
                isSimpleParameterList = false;
                length++;
                break;
            default:
                isSimpleParameterList = false;
                break loop;
        }
    }

    return {
        parameterScope: {
            type: 'AssertedParameterScope',
            paramNames: [],
            hasDirectEval: false,
            isSimpleParameterList
        },
        length
    };
}

function dummyBoundNamesScope() {
    return {
        type: 'AssertedBoundNamesScope',
        boundNames: [],
        hasDirectEval: true
    };
}

function createFunctionContents(obj) {
    let kind = obj.type;

    let directives = obj.body.directives || [];
    delete obj.body.directives;

    let isExpressionBody;
    let body = obj.body;
    delete obj.body;

    if (body.type === 'FunctionBody') {
        body = body.statements;
        isExpressionBody = false;
    } else {
        isExpressionBody = true;
    }

    let contents;

    switch (kind) {
        case 'FunctionDeclaration':
        case 'Method':
            contents = {
                type: 'FunctionOrMethodContents',
                isThisCaptured: false
            };
            break;
        case 'FunctionExpression':
            contents = {
                type: 'FunctionExpressionContents',
                isFunctionNameCaptured: false,
                isThisCaptured: false
            };
            break;
        case 'ArrowExpression':
            if (isExpressionBody) {
                contents = { type: 'ArrowExpressionContentsWithExpression' };
                obj.type = 'ArrowExpressionWithExpression';
            } else {
                contents = { type: 'ArrowExpressionContentsWithFunctionBody' };
                obj.type = 'ArrowExpressionWithFunctionBody';
            }
            break;
        case 'Getter':
            contents = {
                type: 'GetterContents',
                isThisCaptured: false
            };
            break;
        case 'Setter': {
            let { param } = obj;
            delete obj.param;
            let { parameterScope, length } = parameterScopeAndLength([param]);
            contents = {
                type: 'SetterContents',
                isThisCaptured: false,
                param,
                parameterScope
            };
            obj.length = length;
            break;
        }
        default:
            throw new TypeError(`Unexpected function kind ${kind}`);
    }

    if (kind !== 'Getter' && kind !== 'Setter') {
        let { params } = obj;
        delete obj.params;

        contents.params = params;

        let { parameterScope, length } = parameterScopeAndLength(params.items);

        contents.parameterScope = parameterScope;
        obj.length = length;
        obj.isAsync = !!obj.isAsync;
    }

    contents.bodyScope = dummyDeclaredScope('AssertedVarScope');
    contents.body = body;

    obj.contents = contents;
    obj.directives = directives;

    makeEager(obj);
}

function convertObject(obj) {
    switch (obj.type) {
        case 'BlockStatement':
            obj = obj.block;
        // fallthrough to handle the unwrapped `Block` node
        case 'Block':
            obj.scope = dummyDeclaredScope('AssertedBlockScope');
            break;
        case 'ForInStatement':
        case 'ForOfStatement': {
            let { left } = obj;
            if (left.type === 'VariableDeclaration') {
                left.type = 'ForInOfBinding';
                left.binding = left.declarators[0].binding;
                delete left.declarators;
            }
            break;
        }
        case 'FunctionDeclaration':
        case 'Method':
        case 'FunctionExpression':
        case 'ArrowExpression':
        case 'Getter':
        case 'Setter':
            createFunctionContents(obj);
            break;
        case 'LabeledStatement':
            obj.type = 'LabelledStatement';
            break;
        case 'LiteralRegExpExpression': {
            let flags = '';
            if (obj.global) flags += 'g';
            if (obj.ignoreCase) flags += 'i';
            if (obj.multiLine) flags += 'm';
            if (obj.sticky) flags += 'y';
            if (obj.unicode) flags += 'u';
            obj.flags = flags;
            break;
        }
        case 'Script':
            obj.scope = dummyDeclaredScope('AssertedScriptGlobalScope');
            break;
        case 'CatchClause':
            obj.bindingScope = dummyBoundNamesScope();
            break;
        case 'StaticPropertyName':
            obj.type = 'LiteralPropertyName';
            break;
        case 'VariableDeclarationStatement':
            // We don't have a `VariableDeclaration` handler, so, unlike with
            // `BlockStatement`, no need for a fallthrough here.
            obj = obj.declaration;
            break;
    }
    return obj;
}

// See crates/binjs_io/src/escaped_wtf8.rs
function escapeWTF8(s) {
    return s.replace(/[\u007F\uD800-\uDFFF]/gu, m => {
        if (m == '\u007F') {
            return '\u007F007F';
        }
        return '\u007F' + m.charCodeAt(0).toString(16);
    });
}

// This is a replacer callback for JSON.stringify to convert Shift AST to BinaryAST JSON.
module.exports = function convert(_key, value) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        return convertObject(value);
    } else if (typeof value === 'string') {
        return escapeWTF8(value);
    } else {
        return value;
    }
};
