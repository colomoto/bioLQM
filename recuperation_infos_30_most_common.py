import json

f = open("50_namespaces.json", "r");
f2 = open("50_namespaces_reduced.json", "w")

json = json.loads(f.read());

for n in json["_embedded"]["namespaces"]:
    prefix = n["prefix"];
    pattern = n["pattern"];
    namespaceEmbedded = n["namespaceEmbeddedInLui"];
    
    f2.write("- prefix: " + prefix + '\n ' + " pattern: " + pattern + '\n ' + " namespaceEmbedded: \"" + str(namespaceEmbedded).lower() + '\"\n\n');
    
f.close();
f2.close();