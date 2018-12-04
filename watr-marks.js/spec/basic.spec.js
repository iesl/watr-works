"use strict";
/* tslint:disable: no-console */
exports.__esModule = true;
// import * as watrmarks from '../lib/watrmarks';
var watr_1 = require("watr");
describe("importing watr.* modules should work", function () {
    it("import ...", function () {
        var v1 = watr_1.watr.textgrid.TextGridInterop;
        var v2 = watr_1.watr.utils.JsArray;
        expect(true).toEqual(false);
    });
});
