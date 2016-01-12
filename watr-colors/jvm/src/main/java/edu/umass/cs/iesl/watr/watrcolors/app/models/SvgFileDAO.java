package models;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import play.libs.Json;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Data Access Object for SVG and annotation JSON files in a directory. They are paired up where every SVG file has
 * zero or one corresponding JSON file, zero if no annotations exist yet and one if annotations were created for it.
 * <p>
 * TODO should probably be a singleton.
 */
public class SvgFileDAO {

    private String svgDirectory;

    public SvgFileDAO(String svgDirectory) {
        File file = new File(svgDirectory);
        if (!file.exists()) {
            throw new IllegalArgumentException("svg directory not found: " + file);
        }

        this.svgDirectory = svgDirectory;
    }

    public List<Document> getDocuments() throws IOException {
        File[] svgFiles = new File(this.svgDirectory).listFiles((dir, name) -> {
            return name.toLowerCase().endsWith("svg");
        });
        List<Document> docs = new ArrayList<>();
        for (File svgFile : svgFiles) {
            Document doc = new Document(svgFile.getName());
            docs.add(doc);
            createJsonFileIfNotExists(doc);
            File jsonFile = getJsonFile(doc);
            try {
                FileInputStream fileInputStream = new FileInputStream(jsonFile);
                JsonNode annotationsJsonNode = Json.parse(fileInputStream);
                List<Annotation> annotations = Document.deserializeAnnotationListFromJson(annotationsJsonNode);
                doc.addAnnotations(annotations);
            } catch (FileNotFoundException e) {
                System.err.println("error reading JSON annotation file: " + jsonFile + ": " + e.getMessage());
            }
        }
        return docs;
    }

    public Document getDocument(String fileName) throws IOException {
        List<Document> docs = this.getDocuments();
        for (Document doc : docs) {
            if ((fileName != null) && fileName.equals(doc.fileName)) {
                return doc;
            }
        }

        return null;
    }

    public File getJsonFile(Document document) {
        String svgFileName = document.fileName;
        String jsonFileName = svgFileName.substring(0, svgFileName.length() - 4) + ".json";
        return new File(this.svgDirectory, jsonFileName);
    }

    /**
     * Creates an empty .json file corresponding to document if it does not yet exist.
     *
     * @param document
     * @return wasCreated boolean
     */
    public boolean createJsonFileIfNotExists(Document document) throws IOException {
        File jsonFile = getJsonFile(document);
        if (jsonFile.exists()) {
            return false;
        }

        jsonFile.createNewFile();
        ObjectNode emptyObjectNode = Json.newObject();
        BufferedWriter writer = new BufferedWriter(new FileWriter(jsonFile));
        writer.write(emptyObjectNode.toString());
        writer.close();
        System.out.println("created JSON file for document: " + jsonFile);
        return true;
    }

    /**
     * Serializes document's annotations to its corresponding json file.
     *
     * @param document
     */
    public void writeDocument(Document document) throws IOException {
        File jsonFile = getJsonFile(document);
        BufferedWriter writer = new BufferedWriter(new FileWriter(jsonFile));
        writer.write(document.serializeAnnotationsToJson().toString());
        writer.close();
    }

}