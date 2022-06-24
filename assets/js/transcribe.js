function real_transcribe_string(s, map) {
    s = ' ' + s + ' ';
    s = s.normalize();
    let keys = Object.keys(map)
        .filter((x) => x.length > 0)
        .sort((x, y) => y.length - x.length);
    keys.forEach((key) => {
        s = s.replaceAll(key, map[key]);
    });
    return s.substring(1, s.length - 1);
}

function fix_tamil_variants(s) {
    let zipped = s
        .split('')
        .map((c, i) => [s[i - 1] || ' ', c, s[i + 1] || ' ']);
    let result = zipped.map(([previous, current, next]) => {
        if (current !== 'ந') return current;
        if (previous === ' ') return 'ந';
        if (next === 'த') return 'ந';
        return 'ன';
    });
    return result.join('');
}

function apply_replacements(s) {
    for (let key in replacement_words) {
        s = s.replaceAll(
            key.replaceAll('◌', ''),
            replacement_words[key].replaceAll('◌', '')
        );
    }
    return s;
}

function transcribe_string_without_replacements(s, map) {
    // extra whitespace interferes with Tamil cross-word sandhi
    s = s.replaceAll(/\s+/g, ' ');

    // lack of leading whitespace or trailing whitespace interferes with Tamil allophones
    s = ' ' + s + ' ';

    let startingCharacters = ['(', '—', '-', '"', '“'];
    for (let i = 0; i < startingCharacters.length; i++) {
        s = s.replaceAll(startingCharacters[i], startingCharacters[i] + ' ');
    }
    let endingCharacters = [')', '—', '-', '"', '“', ':', '.', '।', ','];
    for (let i = 0; i < endingCharacters.length; i++) {
        s = s.replaceAll(endingCharacters[i], ' ' + endingCharacters[i]);
    }
    let result = real_transcribe_string(s, map);
    for (let i = endingCharacters.length - 1; i >= 0; i--) {
        result = result.replaceAll(
            ' ' + endingCharacters[i],
            endingCharacters[i]
        );
    }
    for (let i = startingCharacters.length - 1; i >= 0; i--) {
        result = result.replaceAll(
            startingCharacters[i] + ' ',
            startingCharacters[i]
        );
    }
    result = result.substring(1, result.length - 1);
    return fix_tamil_variants(result);
}

function transcribe_string_with_replacements(s, map) {
    let result = transcribe_string_without_replacements(s, map);

    if (mapping.to_ipa !== map) {
        result = apply_replacements(result);
    }

    return fix_tamil_variants(result);
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
    var transcribe_string = transcribe_string_with_replacements;
}

if (!replacement_words) {
    var replacement_words = {};
}
