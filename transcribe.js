let to_grantha = {
  'ँ': '𑌁', 'ं': '𑌂', 'ः': '𑌃',
  'अ': '𑌅', 'आ': '𑌆', 'इ': '𑌇', 'ई': '𑌈', 'उ': '𑌉', 'ऊ': '𑌊',
  'ऋ': '𑌋', 'ऌ': '𑌌', 'ए': '𑌏', 'ऐ': '𑌐', 'ओ': '𑌓', 'औ': '𑌔',
  ' ॉ': '𑌾', 'ॆ': '𑍇', 'ॊ': '𑍋', 'ॅ': '𑌾', 'ऑ': '𑌆', 'ऎ': '𑌏', 'ऒ': '𑌓', 'ऍ': '𑌆',
  'क': '𑌕', 'ख': '𑌖', 'ग': '𑌗', 'घ': '𑌘', 'ङ': '𑌙',
  'च': '𑌚', 'छ': '𑌛', 'ज': '𑌜', 'झ': '𑌝', 'ञ': '𑌞',
  'ट': '𑌟', 'ठ': '𑌠', 'ड': '𑌡', 'ढ': '𑌢', 'ण': '𑌣',
  'त': '𑌤', 'थ': '𑌥', 'द': '𑌦', 'ध': '𑌧', 'न': '𑌨',
  'प': '𑌪', 'फ': '𑌫', 'फ़': '𑌫𑌼', 'ब': '𑌬', 'भ': '𑌭', 'म': '𑌮',
  'य': '𑌯', 'र': '𑌰', 'ल': '𑌲', 'ळ': '𑌳', 'व': '𑌵', 'ऴ': '𑌳𑌼',
  'श': '𑌶', 'ष': '𑌷', 'स': '𑌸', 'ह': '𑌹',
  '़': '𑌼', 'ऽ': '𑌽',
  'ा': '𑌾', 'ि': '𑌿', 'ी': '𑍀', 'ु': '𑍁', 'ू': '𑍂',
  'ृ': '𑍃', 'ॄ': '𑍄', 'े': '𑍇', 'ै': '𑍈', 'ो': '𑍋', 'ौ': '𑍌',
  '्': '𑍍',
  'ॐ': '𑍐',
  'ॗ': '𑍗',
  'ॠ': '𑍠', 'ॡ': '𑍡',
  'ॢ': '𑍢', 'ॣ': '𑍣',
  '।': '।', '॥': '॥',
  '०': '௦', '१': '௧', '२': '௨', '३': '௩', '४': '௪', '५': '௫', '६': '௬', '७': '௭', '८': '௮', '९': '௯',
};

let to_brahmi = {
  'ँ': '𑀀', 'ं': '𑀁', 'ः': '𑀂',
  'अ': '𑀅', 'आ': '𑀆', 'इ': '𑀇', 'ई': '𑀈', 'उ': '𑀉', 'ऊ': '𑀊',
  'ऋ': '𑀋', 'ऌ': '𑀍', 'ए': '𑀏', 'ऐ': '𑀐', 'ओ': '𑀑', 'औ': '𑀒',
  ' ॉ': '𑀸', 'ॆ': '𑁂', 'ॊ': '𑁄', 'ॅ': '𑀸', 'ऑ': '𑀆', 'ऎ': '𑀏', 'ऒ': '𑀑', 'ऍ': '𑀆',
  'क': '𑀓', 'ख': '𑀔', 'ग': '𑀕', 'घ': '𑀖', 'ङ': '𑀗',
  'च': '𑀘', 'छ': '𑀙', 'ज': '𑀚', 'झ': '𑀛', 'ञ': '𑀜',
  'ट': '𑀝', 'ठ': '𑀞', 'ड': '𑀟', 'ढ': '𑀠', 'ण': '𑀡',
  'त': '𑀢', 'थ': '𑀣', 'द': '𑀤', 'ध': '𑀥', 'न': '𑀦', 'ऩ': '𑀷',
  'प': '𑀧', 'फ': '𑀨', 'फ़': '𑀨', 'ब': '𑀩', 'भ': '𑀪', 'म': '𑀫',
  'य': '𑀬', 'र': '𑀭', 'ऱ': '𑀶', 'ल': '𑀮', 'ळ': '𑀴', 'ऴ': '𑀵', 'व': '𑀯',
  'श': '𑀰', 'ष': '𑀱', 'स': '𑀲', 'ह': '𑀳',
  '़': '', 'ऽ': '𑀅',
  'ा': '𑀸', 'ि': '𑀺', 'ी': '𑀻', 'ु': '𑀼', 'ू': '𑀽', 'ृ': '𑀾',
  'ॄ': '𑀿', 'े': '𑁂', 'ै': '𑁃', 'ो': '𑁄', 'ौ': '𑁅',
  '्': '𑁆',
  'ॠ': '𑀌', 'ॡ': '𑀎', 'ॢ': '𑁀', 'ॣ': '𑁁',
  '।': '𑁇', '॥': '𑁈',
  '०': '𑁦', '१': '𑁧', '२': '𑁨', '३': '𑁩', '४': '𑁪', '५': '𑁫', '६': '𑁬', '७': '𑁭', '८': '𑁮', '९': '𑁯',
};

let to_tamil = {
  'ँ': 'ஂ', 'ं': 'ஂ', 'ः': '𑌃',
  'अ': 'அ', 'आ': 'ஆ', 'इ': 'இ', 'ई': 'ஈ', 'उ': 'உ', 'ऊ': 'ஊ', 'ऋ': 'ரு\'',
  'ऌ': 'லு\'', 'ए': 'ஏ', 'ऐ': 'ஐ', 'ओ': 'ஓ', 'औ': 'ஔ',
  ' ॉ': 'ா', 'ॆ': 'ெ', 'ॊ': 'ொ', 'ॅ': 'ா', 'ऑ': 'ஆ', 'ऎ': 'எ', 'ऒ': 'ஒ', 'ऍ': 'ஆ',
  'क': 'க', 'ख': 'க²', 'ग': 'க³', 'घ': 'க⁴', 'ङ': 'ங',
  'च': 'ச', 'छ': 'ச²', 'ज': 'ஜ', 'झ': 'ஜ²', 'ञ': 'ஞ',
  'ट': 'ட', 'ठ': 'ட²', 'ड': 'ட³', 'ढ': 'ட⁴', 'ण': 'ண',
  'त': 'த', 'थ': 'த²', 'द': 'த³', 'ध': 'த⁴', 'न': 'ந', 'ऩ': 'ன',
  'प': 'ப', 'फ': 'ப²', 'फ़': 'ஃப', 'ब': 'ப³', 'भ': 'ப⁴', 'म': 'ம',
  'य': 'ய', 'र': 'ர', 'ऱ': 'ற', 'ल': 'ல', 'ळ': 'ள', 'ऴ': 'ழ', 'व': 'வ',
  'श': 'ஶ', 'ष': 'ஷ', 'स': 'ஸ', 'ह': 'ஹ',
  '़': '', 'ऽ': 'அ',
  'ा': 'ா', 'ि': 'ி', 'ी': 'ீ', 'ु': 'ு', 'ू': 'ூ',
  'ृ': '்ரு\'', 'ॄ': '்ரூ\'', 'े': 'ே', 'ै': 'ை', 'ो': 'ோ', 'ौ':'ௌ',
  '्': '்',
  'ॠ': 'ரூ\'', 'ॡ': 'லூ\'', 'ॢ': '்லு\'', 'ॣ': '்லூ\'',
 '।': '.', '॥': '𑿿',
  '०': '௦', '१': '௧', '२': '௨', '३': '௩', '४': '௪', '५': '௫', '६': '௬', '७': '௭', '८': '௮', '९': '௯',
};

let to_latin = {
  'ं': 'ṃ', '◌ँ': 'm̐', 'ः': 'ḥ',
  'अ': 'a', 'आ': 'ā', 'इ': 'i', 'ई': 'ī', 'उ': 'u', 'ऊ': 'ū',
  'ऋ': 'r̥', 'ऌ': 'l̥', 'ए': 'ē', 'ऐ': 'ai', 'ओ': 'ō', 'औ': 'au',
  ' ॉ': '\bô', 'ॆ': '\be', 'ॊ': '\bo', 'ॅ': '\bê', 'ऑ': 'ô', 'ऎ': 'e', 'ऒ': 'o', 'ऍ': 'ê',
  'क': 'ka', 'ख': 'kha', 'ग': 'ga', 'घ': 'gha', 'ङ': 'ṅa',
  'च': 'ca', 'छ': 'cha', 'ज': 'ja', 'झ': 'jha', 'ञ': 'ña',
  'ट': 'ṭa', 'ठ': 'ṭha', 'ड': 'ḍa', 'ढ': 'ḍha', 'ण': 'ṇa',
  'त': 'ta', 'थ': 'tha', 'द': 'da', 'ध': 'dha', 'न': 'na', 'ऩ': 'ṉa',
  'प': 'pa', 'फ': 'pha', 'फ़': 'fa', 'ब': 'ba', 'भ': 'bha', 'म': 'ma',
  'य': 'ya', 'र': 'ra', 'ऱ': 'ṟa', 'ल': 'la', 'ळ': 'ḷa', 'ऴ': 'ḻa', 'व': 'va',
  'श': 'śa', 'ष': 'ṣa', 'स': 'sa', 'ह': 'ha',
  '़': '', 'ऽ': 'a',
  'ा': '\bā', 'ि': '\bi', 'ी': '\bī', 'ु': '\bu', 'ू': '\bū',
  'ृ': '\br̥', 'ॄ': '\br̥̄', 'े': '\bē', 'ै': '\bai', 'ो': '\bō', 'ौ': '\bau',
  '्': '\b',
  'ॠ': 'r̥̄', 'ॡ': 'l̥̄', 'ॢ': '\bl̥', 'ॣ': '\bl̥̄',
  '।': '.', '॥': '.',
  '०': '0', '१': '1', '२': '2', '३': '3', '४': '4', '५': '5', '६': '6', '७': '7', '८': '8', '९': '9',
};

let to_ipa = {
  '◌ँ': '̃', 'ं': '̃', 'ः': 'h',
  'अ': 'ɐ', 'आ': 'aː', 'इ': 'ɪ', 'ई': 'iː', 'उ': 'ʊ', 'ऊ': 'u:',
  'ऋ': 'r̩', 'ऌ': 'l̩', 'ए': 'eː', 'ऐ': 'ɐːi̯', 'ओ': 'oː', 'औ': 'ɐːu̯',
  ' ॉ': '\bɔ', 'ॆ': '\be', 'ॊ': '\bo', 'ॅ': '\bæ', 'ऑ': 'ɔ', 'ऎ': 'e', 'ऒ': 'o', 'ऍ': 'æ',
  'क': 'kɐ', 'ख': 'kʱɐ', 'ग': 'gɐ', 'घ': 'gʱɐ', 'ङ': 'ŋɐ',
  'च': 'tɕɐ', 'छ': 'tɕʱɐ', 'ज': 'dʑɐ', 'झ': 'dʑʱɐ', 'ञ': 'ɲɐ',
  'ट': 'ʈɐ', 'ठ': 'ʈʱɐ', 'ड': 'ɖɐ', 'ढ': 'ɖʱɐ', 'ण': 'ɳɐ',
  'त': 'tɐ', 'थ': 'tʱɐ', 'द': 'dɐ', 'ध': 'dʱɐ', 'न': 'nɐ', 'ऩ': 'n̪ɐ',
  'प': 'pɐ', 'फ': 'pʱɐ', 'फ़': 'fa', 'ब': 'bɐ', 'भ': 'bʱɐ', 'म': 'mɐ',
  'य': 'jɐ', 'र': 'ɾɐ', 'ऱ': 'rɐ', 'ल': 'lɐ', 'ळ': 'ɭɐ', 'ऴ': 'ɻɐ', 'व': 'ʋɐ',
  'श': 'ɕɐ', 'ष': 'ʂɐ', 'स': 'sɐ', 'ह': 'ɦɐ',
  '़': '', 'ऽ': 'ɐ',
  'ा': '\baː', 'ि': '\bɪ', 'ी': '\biː', 'ु': '\bʊ', 'ू': '\buː',
  'ृ': '\br̩', 'ॄ': '\br̩ː', 'े': '\beː', 'ै': '\bɐːi̯', 'ो': '\boː', 'ौ': '\bɐːu̯',
  '्': '\b',
  'ॠ': 'r̩ː', 'ॡ': 'l̩ː', 'ॢ': '\bl̩', 'ॣ': '\bl̩ː',
  '।': '.', '॥': '.',
  '०': '0', '१': '1', '२': '2', '३': '3', '४': '4', '५': '5', '६': '6', '७': '7', '८': '8', '९': '9',
};

function fixup_string_in_tamil(source) {
  let superscripts = "²³⁴";
  let diacritics = "்ாிீுூெேொோைௌ";
  let chars = Array.from(source);
  let n = source.length;
  for (let i = 0; i < n; i++) {
    let c = chars[i];
    if (superscripts.indexOf(c) !== -1) {
      let d = chars[i + 1];
      if (diacritics.indexOf(d) !== -1) {
        chars[i + 1] = c;
        chars[i] = d;
      }
    }
    if (c === "ந" && i > 0) {
      if (chars[i - 1] !== " ") {
        chars[i] = "ன";
      }
    }
  }
  return chars.join("");
}

function fixup_string_in_latin(source) {
  return Array.from(source).filter((x, i) => x !== '\b' && source[i+1] !==
    '\b').join("").replace("\fph", "f");
}

function transcribe_string(string, mapping) {
  let chars = [...string];
  chars = chars.map(x => {
    if (x in mapping) {
      return mapping[x];
    } else {
      return x;
    }
  });
  let s = chars.join("");
  if (mapping == to_tamil) {
    return fixup_string_in_tamil(s);
  }
  if (mapping == to_latin || mapping == to_ipa) {
    return fixup_string_in_latin(s);
  }
  return chars.join("");
}

function fixup_string_in_devanagari(s) {
  return s.replace("फ़",  "\u095e");
}

let originals = new WeakMap();
function transcribe_node(node, mapping) {
  if (node.nodeType === Node.TEXT_NODE) {
    if (!mapping) {
      if (originals.has(node)) {
        node.nodeValue = originals.get(node);
      }
    } else {
      originals.set(node, node.nodeValue);
      node.nodeValue = transcribe_string(fixup_string_in_devanagari(node.nodeValue), mapping);
    }
  } else if (node.nodeType === Node.ELEMENT_NODE) {
    if (node.lang !== "") return;
    for (let i = 0; i < node.childNodes.length; i++) {
      let child = node.childNodes[i];
      transcribe_node(child, mapping);
    }
  }
}

function transcribe(mapping) {
  transcribe_node(document.body, mapping);
  transcribe_node(document.title, mapping);
}

function reset() {
  transcribe(null);
}

function set_script(name) {
  document.getElementById("grantha-name").style.display = "inline";
  document.getElementById("devanagari-name").style.display = "inline";
  document.getElementById("brahmi-name").style.display = "inline";
  document.getElementById("tamil-name").style.display = "inline";
  document.getElementById("latin-name").style.display = "inline";
  document.getElementById("ipa-name").style.display = "inline";

  document.getElementById(name + "-name").style.display = "none";
  script.selectedIndex = ["devanagari", "grantha", "brahmi", "tamil", "latin", "ipa"].indexOf(name);
}

function devanagari() {
  reset();
  document.documentElement.lang = "sa";
  window.history.replaceState("", document.title, window.location.pathname);
  set_script("devanagari");
}

function grantha() {
  reset();
  transcribe(to_grantha);
  document.documentElement.lang = "sa-Gran";
  window.history.replaceState("", document.title, window.location.pathname + "?grantha");
  set_script("grantha");
}

function brahmi() {
  reset();
  transcribe(to_brahmi);
  document.documentElement.lang = "sa-Brah";
  window.history.replaceState("", document.title, window.location.pathname + "?brahmi");
  set_script("brahmi");
}

function tamil() {
  reset();
  transcribe(to_tamil);
  document.documentElement.lang = "sa-Taml";
  window.history.replaceState("", document.title, window.location.pathname + "?tamil");
  set_script("tamil");
}

function latin() {
  reset();
  transcribe(to_latin);
  document.documentElement.lang = "sa-Latn";
  window.history.replaceState("", document.title, window.location.pathname + "?latin");
  set_script("latin");
}

function ipa() {
  reset();
  transcribe(to_ipa);
  document.documentElement.lang = "sa-phonipa";
  window.history.replaceState("", document.title, window.location.pathname + "?ipa");
  set_script("ipa");
}
