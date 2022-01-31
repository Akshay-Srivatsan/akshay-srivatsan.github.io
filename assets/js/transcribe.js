function real_transcribe_string(s, map) {
    s = s.normalize();
    let keys = Object.keys(map)
        .filter((x) => x.length > 0)
        .sort((x, y) => y.length - x.length);
    keys.forEach((key) => {
        s = s.replaceAll(key, map[key]);
    });
    return s;
}

let title = document.title;
let originals = new WeakMap();
function transcribe_node(node, mapping) {
    if (node.nodeType === Node.TEXT_NODE) {
        if (!mapping) {
            if (originals.has(node)) {
                node.nodeValue = originals.get(node);
            }
        } else {
            originals.set(node, node.nodeValue);
            node.nodeValue = transcribe_string(node.nodeValue, mapping);
        }
    } else if (node.nodeType === Node.ELEMENT_NODE) {
        if (node.lang !== '') return;
        if (node.nodeName === 'SCRIPT') return;
        for (let i = 0; i < node.childNodes.length; i++) {
            let child = node.childNodes[i];
            transcribe_node(child, mapping);
        }
    }
}

function transcribe_document(mapping) {
    transcribe_node(document.body, mapping);
    if (mapping) {
        document.title = transcribe_string(title, mapping);
    } else {
        document.title = title;
    }
}

function reset() {
    transcribe_document(null, null);
}

if (!transcribe_string) {
    var transcribe_string = real_transcribe_string;
}
