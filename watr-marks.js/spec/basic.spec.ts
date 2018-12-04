
/* tslint:disable: no-console */

// import * as watrmarks from '../lib/watrmarks';
import { watr } from 'watr';

describe("importing watr.* modules should work", () => {

    it("import ...", () => {
      const v1 = watr.textgrid.TextGridInterop
      const v2 = watr.utils.JsArray

      expect(true).toEqual(false);
    });

});
