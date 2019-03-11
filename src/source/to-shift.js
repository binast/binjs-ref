'use strict';

/**
 * This helper accepts arbitrary BinaryAST node and checks if it represents a function.
 * If it is, it will be modified to conform to corresponding Shift AST interface.
 * If it's not, the node will remain intact and function will bail out.
 * @param {object} obj - a BinaryAST node as a plain JavaScript object.
 */
function tryRemoveFunctionContents(obj) {
    let m = obj.type.match(/^(?:Eager|Lazy)(.*)$/);
    let hasExpressionBody = false;

    switch (m && m[1]) {
        case 'ArrowExpressionWithExpression':
            hasExpressionBody = true;
            // fallthrough
        case 'ArrowExpressionWithFunctionBody':
            obj.type = 'ArrowExpression';
            break;
        case 'FunctionExpression':
        case 'FunctionDeclaration':
        case 'Method':
        case 'Getter':
        case 'Setter':
            obj.type = m[1];
            break;
        default:
            // not a function node, bail out
            return;
    }

    let { params, body } = obj.contents;

    obj.params = params;

    if (!hasExpressionBody) {
        body = {
            type: 'FunctionBody',
            directives: obj.directives,
            statements: body
        };
    }

    obj.body = body;
}

/**
 * Uses the same signature as JSON.parse callback, but limited to objects.
 * @this - The parent object being parsed.
 * @param {string} key - The current key in that object.
 * @param {object} obj - The value being converted and added to the object.
 * @returns {object} Converted node.
 */
function convertObject(key, obj) {
    switch (obj.type) {
        case 'Block':
            switch (this.type) {
                case 'TryCatchStatement':
                case 'TryFinallyStatement':
                case 'CatchClause':
                    // not used as a statement here, do nothing
                    break;
                default:
                    // otherwise wrap
                    obj = {
                        type: 'BlockStatement',
                        block: obj
                    };
                    break;
            }
            break;
        case 'VariableDeclaration':
            if (
                this.type !== 'Export' &&
                !(this.type === 'ForStatement' && key === 'init')
            ) {
                // used as a statement, wrap
                obj = {
                    type: 'VariableDeclarationStatement',
                    declaration: obj
                };
            }
            break;
        case 'LabelledStatement':
            obj.type = 'LabeledStatement';
            break;
        case 'LiteralPropertyName':
            obj.type = 'StaticPropertyName';
            break;
        case 'LiteralRegExpExpression': {
            let flags = obj.flags;
            obj.global = flags.includes('g');
            obj.ignoreCase = flags.includes('i');
            obj.multiLine = flags.includes('m');
            obj.sticky = flags.includes('y');
            obj.unicode = flags.includes('u');
            break;
        }
        case 'ForInOfBinding':
            obj.type = 'VariableDeclaration';
            obj.declarators = [
                {
                    type: 'VariableDeclarator',
                    init: null,
                    binding: obj.binding
                }
            ];
            break;
        default:
            tryRemoveFunctionContents(obj);
            break;
    }

    return obj;
}

/**
 * This is a reviver callback for JSON.parse to convert Binary AST JSON to Shift AST.
 * @this - The parent object being parsed.
 * @param {string} key - The current key in that object.
 * @param {*} value - The value being converted and added to the object.
 * @returns {*} Converted value.
 */
module.exports = function convert(key, value) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        return convertObject.call(this, key, value);
    } else {
        return value;
    }
};
