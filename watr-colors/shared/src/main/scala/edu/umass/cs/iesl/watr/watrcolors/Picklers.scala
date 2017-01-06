package edu.umass.cs.iesl.watr
package watrcolors


trait AllPicklers
    extends textreflow.TextReflowBoopicklers
    with HtmlUpdatePicklers
    with RemoteCallPicklers




// implicit val pComponent = generatePickler[Component]

// pSharedJs
//   .addConcreteType[PageGeometry]
//   .addConcreteType[TargetRegion]
//   .addConcreteType[TargetFigure]
//   .addConcreteType[Label]
//   .addConcreteType[Zone]
//   .addConcreteType[Component]

// import tracing._
// import TraceLog._
// implicit val pDSL = compositePickler[TraceLog]
// implicit val pSetPageGeometry = generatePickler[SetPageGeometries]
// implicit val pShow            = generatePickler[Show]
// implicit val pShowZone        = generatePickler[ShowZone]
// // implicit val pShowComponent   = generatePickler[ShowComponent]
// // implicit val pShowLabel       = generatePickler[ShowLabel]
// implicit val pFocusOn         = generatePickler[FocusOn]
// implicit val pIndicate        = generatePickler[Indicate]
// // implicit val pMessage         = generatePickler[Message]
// implicit val pAll             = generatePickler[All]
// implicit val pLink            = generatePickler[Link]
// implicit val pGroup           = generatePickler[Group]
// implicit val pGroupEnd        = generatePickler[GroupEnd]

// pDSL
//   .addConcreteType[SetPageGeometries]
//   .addConcreteType[Show]
//   .addConcreteType[ShowZone]
//   .addConcreteType[FocusOn]
//   .addConcreteType[Indicate]
//   .addConcreteType[All]
//   .addConcreteType[Link]
//   .addConcreteType[Group]
//   .addConcreteType[GroupEnd]
// .addConcreteType[Message]
// .addConcreteType[ShowComponent]
// .addConcreteType[ShowLabel]

// implicit val pc1 = compositePickler[TextReflowF[_]]
// implicit val p11: Pickler[Atom]    = generatePickler[Atom]
// implicit val p12: Pickler[Insert]  = generatePickler[Insert]
// implicit def p13[A: P]: P[Rewrite[A]] = generatePickler[Rewrite[A]]
// implicit def p14[A: P]: P[Bracket[A]] = generatePickler[Bracket[A]]
// implicit def p15[A: P]: P[Flow[A]]    = generatePickler[Flow[A]]
// implicit def p16[A: P]: P[Labeled[A]] = generatePickler[Labeled[A]]


// pc1
//   .addConcreteType[Atom]
//   .addConcreteType[Insert]
// .addConcreteType[Rewrite[_]]
// .addConcreteType[Bracket[_]]
// .addConcreteType[Flow[_]]
// .addConcreteType[Labeled[_]]
