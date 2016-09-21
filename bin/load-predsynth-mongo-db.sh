#!/bin/bash

# D=`date +'%s'`
# M=openreview-prod-$D
# F=openreview-prod-$D.tgz

# ssh vinci8.cs.umass.edu <<EOF
# cd /tmp
# mongodump --db 'openreview-production' --out "$M"
# tar cvzf $F $M
# rm -rf $M
# EOF

# scp vinci8:/tmp/$F ~/openreview-mongodbs

# ssh vinci8.cs.umass.edu <<EOF
# cd /tmp
# rm -f $F
# EOF

# cd ~/openreview-mongodbs
# tar xzvf $F


mongo localhost/predsynth --eval 'db.dropDatabase()'
# mongorestore --drop --db predsynth $M/openreview-production

declare -a collections=(
    annotated_amounts
    annotated_apparatuses
    annotated_conditions
    annotated_connections
    annotated_entdescriptors
    annotated_entities
    annotated_operations
    annotated_papers
    annotated_paragraphs
    annotated_rawtext_snippets
    chemical_blacklist
    chemical_db
    doi_title_abstract_map
    entity_properties
    interpreted_papers
    papers
    parsed_papers
    test_papers
    things_keys
    training_papers
)

declare -a collsOfInterest=(
    annotated_amounts
    annotated_apparatuses
    annotated_conditions
    annotated_entities
    annotated_operations
    annotated_papers
    annotated_paragraphs
    chemical_blacklist
    chemical_db
)

for c in "${collsOfInterest[@]}"; do
    mongorestore --drop --db predsynth predsynth/$c.bson
done
