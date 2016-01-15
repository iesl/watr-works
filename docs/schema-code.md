
```python

    class Recipe(Document):

      structure = {
        'paper_dois': [unicode],    #IDs of papers which contain this recipe
        'entity_ids': [unicode],    #set of entities contained in this recipe
        'operation_ids': [unicode], #set of operations contained in this recipe
        'paragraph_ids': [unicode], #set of paragraphs used to construct this recipe
        'sequence': [
          {
            'input_entity_ids': [unicode],


* This has to point to a span of text, not be human-parsed and hand entered. Any unit translations have to be performed by the
  inference system, otherwise it is meaningless to label them.
  Some increasingly difficult cases I see are:
  1. entity and quantity are directly specifed, e.g., 20mg of X
  2. quantities of multiple inputs are jointly specified ('stoichiometric amounts', 'molar ratio 1.0:3.5')
  3. quantities are indirectly specified ('literature', 'standard')

            'input_amounts': dict,  #Format: { 'entity_id': [ ['<amount>', '<units>'] ] }

* The span has to have the operation and amounts independently labeled
            'raw_texts': [unicode], #IDs of raw text snippets for operation (which should contain the amounts too)

            'traversals': int,      #number of times to perform this operation
            'operation_id': unicode,
            'output_entity_ids': [unicode]
          }
        ]
      }


    class Operation(Document):
      Models a unit operation performed on a set of entities (materials)

      structure = {
        'paper_dois': [unicode],    #  IDs of papers which contain this operation
        'paragraph_ids': [unicode], #  IDs of paragraphs extracted from
        'recipe_ids': [unicode],    #  IDs of recipes where this operation shows up
        'type': unicode,            #  operation type (heat, grind, mix, stir, etc.)
        'conditions': {
* The annotator should be annotating the provided unit ('Celsius'), and letting the computer calculate K
          'temperature': float,     #  temperature in Kelvin

* Again, annotate the provided unit or scale type:
          'heat_rate': float,       #  temperature rate in Kelvin / hr
          'pH': float,              #  pH
          'pressure': float,        #  pressure in bar
          'time': float,            #  time in seconds
          'rot_speed': float,       #  speed in rpm
          'misc': [unicode]         #  Any other miscellaneous conditions in arbitrary string format
        },
* Sometimes both 'type' and 'apparatuses' can be annotated, but frequently just an apparatus is specified, 
  in which case only this field should be specified
        'apparatuses': [unicode],   #  apparatuses used
        
* (Emma: Don't understand what is being annotated here?)
        'raw_texts': [],            #  parse subtrees of operation; RawTextSnippet IDs
      }

    class RawTextSnippet(Document):
      Models a snippet of RawText, extracted from a recipe paragraph and mapped to an entity/operation

      structure = {
        'paper_doi': unicode,
        'paragraph_id': unicode,
        'entity_id': unicode,
        'target_entity_id': unicode,
        'operation_id': unicode,
        'raw_text': unicode,
        'start_char_index': int, #Position in paragraph of 1st character in snippet
        'char_length': int #Number of characters in rawtext snippet
      }


    class Entity(Document):
      Models a unique chemical identity

      structure = {
        'mpids' : [unicode], #Materials Project IDs corresponding to this entity
        'aliases': [unicode], #unique names, formulas and all aliases
        'raw_texts': [], #raw text strings of entities; RawTextSnippet IDs
        'feature_vector': [], #feature vector for ML purposes
        'physical_data': dict, #key-value of any pulled physical data
        'composition': dict, #key-value of either concentrations (e.g. element percents in solvent) or alloys (i.e. element percents)
        'structure': str, #physical structure, e.g. nanowire
        'paper_dois': [unicode], #IDs of papers which contain this entity
        'paragraph_ids': [unicode], #IDs of paragraphs extracted from
        'recipe_ids': [unicode], #IDs of recipes where this entity shows up
      }

    class TargetEntity(Entity):
      Models a 'target' entity for a paper; subclass of Entity that just defaults to a different collection




    class Paragraph(Document):
      Models a paragraph belonging to an article

      structure = {
        'doi' : unicode, #doi of article the paragraph belongs to
        'order': int, #which paragraph in the paper (i.e. sequence); 0-indexed
        'feature_vector': [int], #calculated feature vector of Paragraph
        'text': unicode, #stringified version of the paragraph
        'is_recipe': bool #if it is a recipe
      }

    class Paper(Document):
      Models an article

      structure = {
        'doi': unicode,
        'pdf_loc': unicode,
        'title': unicode,
        'abstract': unicode,
        'target_entity_ids': [unicode],
        'plaintext': unicode,
        'recipe_ids': [unicode]
      }


      def get_num_operations(self)               : Gets number of operations in the paper
      def get_operations(self)                   : Gets operations mentioned in a paper
      def get_num_entities(self)                 : Gets number of entities in the paper
      def get_entities(self)                     : Gets entities mentioned in a paper
      def get_num_target_entities(self)          : Gets number of target entities in the paper
      def get_target_entities(self)              : Gets target entities mentioned in a paper
      def get_num_paragraphs(self)               : Gets number of paragraphs in the paper
      def get_paragraphs(self)                   : Gets all paragraphs belonging to this paper
      def delete(self)                           : Deletes this paper and associated objects (paragraphs, entities, etc.) from the database
      def delete_paragraphs(self)                : Deletes just the paragraphs
      def delete_extracted_operations(self)      : Deletes just the extracted operations
      def delete_extracted_entities(self)        : Deletes just the extracted entities
      def delete_extracted_target_entities(self) : Deletes just the extracted target entities
      def delete_extracted_recipes(self)         : Deletes just the extracted recipes
      def delete_extracted_rawtexts(self)        : Deletes just the extracted raw texts

    class AnnotatedPaper(Document):
      Similar to the Paper class, but some differences to make manual annotation simpler.
      Namely, these are fully encapsulated/nested data structures, to avoid cluttering/polluting DB collections in Mongo.


      structure = {
        'doi': unicode,
        'pdf_loc': unicode,
        'title': unicode,
        'abstract': unicode,
        'target_entity_ids': [unicode],
        'plaintext': unicode,
        'recipe_ids': [unicode],

        'paragraphs': [],
        'entities': [],
        'target_entities': [],
        'operations': [],
        'recipes': [],
        'rawtext_snippets': []
      }



```
