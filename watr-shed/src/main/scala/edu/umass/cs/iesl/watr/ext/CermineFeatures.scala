package edu.umass.cs.iesl.watr
package ext

import watrmarks._

object CermineFeatures {
  import SpatialEnrichments._


  def textDensity(zoneIter: ZoneIterator): Double = {

    // add up area of each token (a bit rougher approx. than char level)
    val tokenArea: Double = zoneIter.getTokens.map(_._1.area).sum
    val zoneArea = zoneIter.currentZone.area

    tokenArea / zoneArea
  }


  def charCountFeature(zoneIter: ZoneIterator): Double = {
    val charCount = zoneIter
      .getText.toCharArray
      .foldLeft(0)({
        case (acc, c) =>
          if (c.isSpaceChar) acc else acc+1
      })

    charCount.toDouble
  }

  def lineHeightMaxMeanFeature(zoneIter: ZoneIterator): Double = {

    // double zoneMean = 0;
    // for (BxLine line : zone) {
    //     zoneMean += line.getBounds().getHeight();
    // }
    // zoneMean /= (double) zone.childrenCount();
    // for (BxZone z : page) {
    //     if (z.equals(zone)) {
    //         continue;
    //     }
    //     double pageMean = 0;
    //     for (BxLine line : z) {
    //         pageMean += line.getBounds().getHeight();
    //     }
    //     pageMean /= z.childrenCount();
    //     if (pageMean > zoneMean + 1) {
    //         return 0;
    //     }
    // }
    // return 1;
    ???
  }






  //   def lineHeightMeanFeature(zone: BxZone, page: BxPage): Double = {
  //         double mean = 0;
  //         for (BxLine line : zone) {
  //             mean += line.getHeight();
  //         }
  //         return mean / (double) zone.childrenCount();
  //     }





  //   def bibinfoFeature(zone: BxZone, page: BxPage): Double = {

  //         String[] keywords = {"cite", "pages", "article", "volume", "publishing", "journal", "doi", "cite this article",
  //                              "citation", "issue", "issn"};

  //         String[] otherKeywords = {"author details", "university", "department", "school", "institute", "affiliation",
  //                              "hospital", "laboratory", "faculty", "author", "abstract", "keywords", "key words",
  //                              "correspondence", "editor", "address", "email"};


  //         int count = 0;
  //         for (String keyword : keywords) {
//             if (zone.toText().toLowerCase().contains(keyword)) {
//                 count += 2;
//             }
//         }
//         for (String keyword : otherKeywords) {
//             if (count > 0 && zone.toText().toLowerCase().contains(keyword)) {
//                 count--;
//             }
//         }

//         return (double)count / 2;
//     }


//   def charCountRelativeFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     count += chunk.toText().length();
//                 }
//             }
//         }

//         int pCount = 0;
//         for (BxZone pZone : page) {
//             for (BxLine line : pZone) {
//                 for (BxWord word : line) {
//                     for (BxChunk chunk : word) {
//                         pCount += chunk.toText().length();
//                     }
//                 }
//             }
//         }
//         return (double) count / (double) pCount;
//     }





//   def commaCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         if (arr[i]==',') {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count;
//     }





//   def commaRelativeCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         int allCount = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         allCount++;
//                         if (arr[i]==',') {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count / (double) allCount;
//     }





//     private static String featureName = "ContainsCuePhrases";
//     private static String[] cuePhrases = {"although", "therefore", "therein", "hereby",
//         "nevertheless", "to this end", "however", "moreover", "nonetheless"};

//     public String getFeatureName() {
//         return featureName;
//     }

//   def ContainsCuePhrasesFeature(zone: BxZone, page: BxPage): Double = {
//         String zoneText = zone.toText().toLowerCase();

//         for (String cuePhrase : cuePhrases) {
//             if (!zoneText.contains(cuePhrase)) {
//                 continue;
//             } else {
//                 return 1.0;
//             }
//         }
//         return 0.0;
//     }





//   def containsPageNumberFeature(zone: BxZone, page: BxPage): Double = {
//         if (zone.childrenCount() > 1) {
//             return 0;
//         }
//         if (Pattern.matches("^\\d+$|^Page\\s+.*$|^page\\s+.*$", zone.toText())) {
//             return 1;
//         }
//         return 0;
//     }






//   def contributionFeature(zone: BxZone, page: BxPage): Double = {
//         String[] keywords = {"contribution"};

//         for (String keyword : keywords) {
//             if (zone.toText().toLowerCase().contains(keyword)) {
//                 return 1;
//             }
//         }
//         return 0;
//     }





//   def correspondenceFeature(zone: BxZone, page: BxPage): Double = {
//         String[] keywords = {"addressed", "correspondence", "email", "address"};

//         int count = 0;
//         for (String keyword : keywords) {
//             if (zone.toText().toLowerCase().contains(keyword)) {
//                 count++;
//             }
//         }

//         return count;
//     }






//   def dotCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         if (arr[i]=='.') {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count;
//     }





//   def dotRelativeCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         int allCount = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         allCount++;
//                         if (arr[i]=='.') {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count / (double) allCount;
//     }






// 	def figureFeature(zone: BxZone, page: BxPage): Double = {
// 		String[] keywords = { "figure", "fig.", "table", "tab." };

// 		for (String keyword : keywords) {
// 			if (zone.toText().toLowerCase().startsWith(keyword)) {
// 				return 1;
// 			}
// 		}
// 		return 0;
// 	}






//   def figureTableFeature(zone: BxZone, page: BxPage): Double = {
//         int i = 0;
//         for (BxLine line : zone) {
//             String text = line.toText().toLowerCase();
//             if (text.matches("figure ?[0-9ivx]+[\\.:].*$") || text.matches("table ?[0-9ivx]+[\\.:].*$")
//                     || text.matches("figure ?[0-9ivx]+$") || text.matches("table ?[0-9ivx]+$")) {
//                 if (i == 0) {
//                     return 1;
//                 }
//                 if (Math.abs(line.getX() - line.getPrev().getX()) > 5) {
//                     return 1;
//                 }
//                 double prevW = 0;
//                 for (BxWord w : line.getPrev()) {
//                     for (BxChunk ch : w) {
//                         prevW += ch.getArea();
//                     }
//                 }
//                 prevW /= Math.max(line.getPrev().getArea(), line.getArea());
//                 double lineW = 0;
//                 for (BxWord w : line) {
//                     for (BxChunk ch : w) {
//                         prevW += ch.getArea();
//                     }
//                 }
//                 lineW /= Math.max(line.getPrev().getArea(), line.getArea());
//                 if (Math.abs(lineW -prevW) < 0.3) {
//                     return 1;
//                 }
//                 return 0.3;
//             }
//             i++;
//         }
//         return 0;
//     }







// 	def freeSpaceWithinZoneFeature(zone: BxZone, page: BxPage): Double = {
// 		double charSpace = 0.0;

// 		for (BxLine line : zone) {
// 			for (BxWord word : line) {
// 				for (BxChunk chunk : word) {
// 					charSpace += chunk.getArea();
// 				}
// 			}
// 		}
// 		return zone.getArea() - charSpace;
// 	}






//   def greekLettersFeature(zone: BxZone, page: BxPage): Double = {
//         return (zone.toText().matches("^.*[\\u0391-\\u03A9].*$") ||
//                 zone.toText().matches("^.*[\\u03B1-\\u03C9].*$")) ? 1: 0;
//     }






//   def keywordsFeature(zone: BxZone, page: BxPage): Double = {
//         String[] keywords = {"keywords", "key words", "index terms"};

//         for (String keyword : keywords) {
//             if (zone.toText().toLowerCase().startsWith(keyword)) {
//             	return 1.0;
//             }
//         }
//         return 0.0;
//     }






// 	def lastButOneZoneFeature(zone: BxZone, page: BxPage) {
// 		if (object.hasPrev()) {
// 			if (object.getPrev().hasPrev()) {
// 				return (double) object.getPrev().getPrev().getLabel().ordinal();
// 			}
// 		}
// 		return -1.0;
// 	}





//   def letterCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         if (Character.isLetter(arr[i])) {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count;
//     }





//   def letterRelativeCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         int allCount = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         allCount++;
//                         if (Character.isLetter(arr[i])) {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count / (double) allCount;
//     }





//   def licenseFeature(zone: BxZone, page: BxPage): Double = {
//         String[] keywords = {"terms", "distributed", "reproduction", "open", "commons",
//             "license", "©", "creative", "copyright", "cited", "distribution", "access"};

//         int count = 0;
//         for (String keyword : keywords) {
//             if (zone.toText().toLowerCase().contains(keyword)) {
//                 count++;
//             }
//         }

//         return count;
//     }






//   def lineCountFeature(zone: BxZone, page: BxPage): Double = {
//         return (double) zone.childrenCount();
//     }










//   def lineRelativeCountFeature(zone: BxZone, page: BxPage): Double = {
//         int allLines = 0;
//         for (BxZone pZone : page) {
//             allLines += pZone.childrenCount();
//         }

//         return (double) zone.childrenCount() / (double) allLines;
//     }






//   def lineWidthMeanFeature(zone: BxZone, page: BxPage): Double = {
//         double mean = 0;
//         for (BxLine line : zone) {
//             mean += line.getBounds().getWidth();
//         }
//         return mean / (double) zone.childrenCount();
//     }






//   def lineXPositionDiffFeature(zone: BxZone, page: BxPage): Double = {
//         double min = zone.getBounds().getX() + zone.getBounds().getWidth();
//         double max = zone.getBounds().getX();
//         for (BxLine line : zone) {
//             if (line.getBounds().getX() < min) {
//                 min = line.getBounds().getX();
//             }
//             if (line.getBounds().getX() > max) {
//                 max = line.getBounds().getX();
//             }
//         }
//         return max - min;
//     }






//   def lineXPositionMeanFeature(zone: BxZone, page: BxPage): Double = {
//         double mean = 0;
//         for (BxLine line : zone) {
//             mean += line.getBounds().getX();
//         }
//         return mean / (double) zone.childrenCount() - zone.getBounds().getX();
//     }






//   def lineXWidthPositionDiffFeature(zone: BxZone, page: BxPage): Double = {
//         double min = zone.getBounds().getX() + zone.getBounds().getWidth();
//         double max = zone.getBounds().getX();
//         for (BxLine line : zone) {
//             if (line.getBounds().getX()+line.getBounds().getWidth() < min) {
//                 min = line.getBounds().getX()+line.getBounds().getWidth();
//             }
//             if (line.getBounds().getX()+line.getBounds().getWidth() > max) {
//                 max = line.getBounds().getX()+line.getBounds().getWidth();
//             }
//         }
//         return max - min;
//     }






//   def mathSymbolsFeature(zone: BxZone, page: BxPage): Double = {
//         return (zone.toText().matches("^.*[=\\u2200-\\u22FF].*$")) ? 1 : 0;
//     }

// 	def pageNumberFeature(zone: BxZone, page: BxPage): Double = {
// 		return (double)Integer.valueOf(context.getId());
// 	}






//   def referencesFeature(zone: BxZone, page: BxPage): Double = {
//         int refDigits = 0;
//         int refIndents = 0;
//         for (BxLine line : zone) {
//             if (Pattern.matches("^\\[\\d+\\].*", line.toText()) || Pattern.matches("^\\d+\\..*", line.toText())) {
//                 refDigits++;
//             }
//             if (zone.getBounds().getX() + 8 < line.getBounds().getX()) {
//                 refIndents++;
//             }
//         }
//         return ((double)refDigits > (double)zone.childrenCount() / 4.0
//                 || (double)refIndents > (double)zone.childrenCount() / 4.0) ? 1 : 0;
//     }






//   def referencesTitleFeature(zone: BxZone, page: BxPage): Double = {
//         String[] keywords = {"referen", "biblio"};

//         for (String keyword : keywords) {
//             if (zone.toText().toLowerCase().startsWith(keyword)) {
//                 return 1;
//             }
//         }
//         return 0;
//     }






//   def relativeMeanLengthFeature(zone: BxZone, page: BxPage): Double = {
//         BxLine firstLine = zone.getFirstChild();
//         BxLine line = firstLine;
//         double meanTotalWidth = line.getWidth();
//         int lineCount = 1;

//         while (line.hasPrev()) {
//             line = line.getPrev();
//             meanTotalWidth += line.getWidth();
//             lineCount++;
//         }
//         line = firstLine;
//         while (line.hasNext()) {
//             line = line.getNext();
//             meanTotalWidth += line.getWidth();
//             lineCount++;
//         }

//         if (lineCount == 0) {
//             return 0;
//         }

//         meanTotalWidth /= lineCount;

//         double meanZoneWidth = 0;
//         for (BxLine l : zone) {
//             meanZoneWidth += l.getWidth();
//         }

//         if (!zone.hasChildren() || meanTotalWidth == 0) {
//             return 0;
//         }

//         return meanZoneWidth / zone.childrenCount() / meanTotalWidth;
//     }







//   def startsWithDigitFeature(zone: BxZone, page: BxPage): Double = {
//     	if (zone.toText().length() == 0) {
//     		return 0.0;
//     	} else {
//     		return (Character.isDigit(zone.toText().charAt(0))) ? 1.0 : 0.0;
//     	}
//     }






// 	def StartsWithHeaderFeature(zone: BxZone, page: BxPage): Double = {
// 		BxLine firstLine = zone.getFirstChild();
// 		String lineText = firstLine.toText();
// 		String text = zone.toText();

// 		String itemizeString = "";
// 		itemizeString += "|^\\d+\\.\\d+\\.\\s+\\p{Upper}.+";
// 		itemizeString += "|^\\d+\\.\\s+\\p{Upper}.+";
// 		itemizeString += "|^\\p{Upper}\\.\\s[^\\.]+";
// 		itemizeString += "|^\\p{Lower}\\)\\s+.+";
// 		Pattern itemizePattern = Pattern.compile(itemizeString);

// 		String subpointsString = "";
// 		subpointsString += "^\\d\\.\\d\\.\\s+\\p{Upper}.+";
// 		subpointsString += "|^\\d\\.\\d\\.\\d\\.\\s+\\p{Upper}.+";
// 		Pattern subpointsPattern = Pattern.compile(subpointsString, Pattern.DOTALL); //for multiline matching

// 		Matcher matcher1 = itemizePattern.matcher(text);
// 		Matcher matcher2 = subpointsPattern.matcher(text);

// 		if(matcher1.matches() || matcher2.matches()) {
// 			return 1.0;
// 		}

// 		if (zone.childrenCount() <= 2) {
// 			return 0;
// 		}
// 		if (!lineText.contains(" ") && !lineText.matches(".*\\d.*") && lineText.matches("\\p{Upper}.+")) {
// 			return 1;
// 		}
// 		String[] words = lineText.split(" ");
// 		boolean capitals = true;
// 		for (String word : words) {
// 			if (!(word.matches("\\{Upper}.+") || ZoneClassificationUtils.isConjunction(word))) {
// 				capitals = false;
// 				break;
// 			}
// 		}

//         return capitals ? 1.0 : 0;
// 	}





//   def uppercaseCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         if (Character.isUpperCase(arr[i])) {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count;
//     }





//   def uppercaseRelativeCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         int allCount = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 for (BxChunk chunk : word) {
//                     char[] arr = chunk.toText().toCharArray();
//                     for (int i = 0; i < arr.length; i++) {
//                         allCount++;
//                         if (Character.isUpperCase(arr[i])) {
//                             count++;
//                         }
//                     }
//                 }
//             }
//         }
//         return (double) count / (double) allCount;
//     }





//   def uppercaseWordCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 StringBuilder sb = new StringBuilder();
//                 for (BxChunk chunk : word) {
//                     sb.append(chunk.toText());
//                 }
//                 String s = sb.toString();
//                 if (!s.isEmpty() && Character.isUpperCase(s.charAt(0))) {
//                     count++;
//                 }
//             }
//         }
//         return (double) count;
//     }






//   def uppercaseWordRelativeCountFeature(zone: BxZone, page: BxPage): Double = {
//         int count = 0;
//         int allCount = 0;
//         for (BxLine line : zone) {
//             for (BxWord word : line) {
//                 allCount++;
//                 if (word.hasChildren()) {
//                 	String s = word.getChild(0).toText();
//                 	if (!s.isEmpty() && Character.isUpperCase(s.charAt(0))) {
//                 		count++;
//                 	}
//                 }
//             }
//         }
//         return (double) count / (double) allCount;
//     }





//     private static final double ZONE_EPSILON = 1.0;

//   def verticalProminenceFeature(zone: BxZone, page: BxPage): Double = {
//         if (page.childrenCount() == 1) {
//             return 0.0; //there is only one zone - no prominence can be measured
//         }
//         BxZone prevZone = zone.getPrev();
//         BxZone nextZone = zone.getNext();
//         while (true) {
//             if (prevZone == null) { //given zone is the first one in the set - there is none before it
//                 if (nextZone == null) {
//                     return page.getHeight() - zone.getHeight();
//                 } else if (nextZone.getY() - (zone.getY() + zone.getHeight()) > ZONE_EPSILON) {
//                     return nextZone.getY() - (zone.getY() + zone.getHeight());
//                 } else {
//                     nextZone = nextZone.getNext();
//                     continue;
//                 }
//             } else if (nextZone == null) { //given zone is the last one in the set - there is none after it
//                 if (zone.getY() - (prevZone.getY() + prevZone.getHeight()) > ZONE_EPSILON) {
//                     return zone.getY() - (prevZone.getY() + prevZone.getHeight());
//                 } else {
//                     prevZone = prevZone.getPrev();
//                     continue;
//                 }
//             } else { //there is a zone before and after the given one
//                 if (zone.getY() - (prevZone.getY() + prevZone.getHeight()) > ZONE_EPSILON) { //previous zone lies in the same column
//                     if (nextZone.getY() - (zone.getY() + zone.getHeight()) > ZONE_EPSILON) { //next zone lies in the same column
//                         return nextZone.getY() - (prevZone.getY() + prevZone.getHeight()) - zone.getHeight();
//                     } else {
//                         nextZone = nextZone.getNext();
//                         continue;
//                     }
//                 } else {
//                     if (nextZone.getY() - (zone.getY() + zone.getHeight()) > ZONE_EPSILON) {
//                         prevZone = prevZone.getPrev();
//                         continue;
//                     } else { //neither previous zone nor next zone lies in natural geometrical order
//                         prevZone = prevZone.getPrev();
//                         nextZone = nextZone.getNext();
//                         continue;
//                     }
//                 }
//             }
//         }
//     }





//   def widthFeature(zone: BxZone, page: BxPage): Double = {
//         return zone.getBounds().getWidth();
//     }


//   def widthRelativeFeature(zone: BxZone, page: BxPage): Double = {
//     	if (page.getBounds().getWidth() < 0.00001) {
//     		return 0.0;
//     	}
//         return zone.getBounds().getWidth() / page.getBounds().getWidth();
//     }

}
