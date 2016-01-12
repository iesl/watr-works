import models.Annotation;
import models.Document;
import models.Rectangle;
import models.SvgFileDAO;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class SvgFileDAOTest {
    @Test
    public void testCreation() throws IOException {
        // non-existent svgDirectory
        try {
            new SvgFileDAO("/tmp/non-existent-dir");
            fail("constructor should fail for non-existent svgDirectory");
        } catch (Exception ignored) {
        }

        // no svg files
        Path tempDir = Files.createTempDirectory("svg-test");
        SvgFileDAO svgFileDAO = new SvgFileDAO(tempDir.toAbsolutePath().toString());
        assertEquals(0, svgFileDAO.getDocuments().size());

        // one svg file w/no annotations
        File svgFile = Files.createTempFile(tempDir, null, ".svg").toFile();
        svgFileDAO = new SvgFileDAO(tempDir.toAbsolutePath().toString());
        List<Document> docs = svgFileDAO.getDocuments();    // creates corresponding .json file
        assertEquals(1, docs.size());                       // found the one svg file

        Document doc1 = docs.get(0);
        String svgFileName = svgFile.getName();
        String expJsonFileName = svgFileName.substring(0, svgFileName.length() - 4) + ".json";
        File jsonFile = svgFileDAO.getJsonFile(doc1);
        assertEquals(expJsonFileName, jsonFile.getName());

        doc1 = docs.get(0);
        assertEquals(svgFile.getName(), doc1.fileName);
        assertEquals(0, doc1.getAnnotations().size());

        // create and save some annotations
        Annotation annot1 = new Annotation("label 1", Arrays.asList(new Rectangle(0, 1, 2, 3)));
        Annotation annot2 = new Annotation("label 2", Arrays.asList(new Rectangle(4, 5, 6, 7)));
        doc1.addAnnotation(annot1);
        doc1.addAnnotation(annot2);
        svgFileDAO.writeDocument(doc1);

        docs = svgFileDAO.getDocuments();   // reloads
        assertEquals(1, docs.size());

        doc1 = docs.get(0);
        List<Annotation> doc1Annots = doc1.getAnnotations();
        assertEquals(2, doc1Annots.size());
        // TODO cannot depend on order?
        assertEquals(annot1, doc1Annots.get(0));
        assertEquals(annot2, doc1Annots.get(1));
    }
}