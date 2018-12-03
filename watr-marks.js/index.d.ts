

declare namespace watr.utils {
  export interface JsArray {
  }
}

declare namespace textgrid {


}

declare namespace TextGridInterop {
  namespace widgetDisplayGridProps {

    export function gridRegions(gridProps: any): any;
  }

  namespace labelSchemas {

    export function allLabels(labelSchema: any): any;
  }
}


declare module "watr" {
  // export = utils;
}
declare module "watr.utils" {
  // export = utils;
}

declare module "watr.scalazed" {
  class Node {}
  class Leaf {}
  type Tree = Node | Leaf;

  namespace Tree {
    function Node(label: any, ch: Tree[]): Tree;
    function Leaf(label: any): Tree;

    function drawTree(desc: Tree): string;
  }
}

declare module "watr.textgrid.TextGridInterop" {
  export = TextGridInterop;
}


// export as namespace watr;

// export namespace watr {


// }
// declare namespace watr {

//   namespace utils {
//     interface JSArray {
//     }
//   }

//   namespace textgrid {

//     interface TextGridInterop {
//     }

//   }
// }
