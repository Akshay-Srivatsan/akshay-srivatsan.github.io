let to_grantha = {
  'ऀ': '𑌀',
  'ँ': '𑌁',
  'ं': '𑌂',
  'ः': '𑌃',
  'अ': '𑌅',
  'आ': '𑌆',
  'इ': '𑌇',
  'ई': '𑌈',
  'उ': '𑌉',
  'ऊ': '𑌊',
  'ऋ': '𑌋',
  'ऌ': '𑌌',
  'ए': '𑌏',
  'ऐ': '𑌐',
  'ओ': '𑌓',
  'औ': '𑌔',
  'क': '𑌕',
  'ख': '𑌖',
  'ग': '𑌗',
  'घ': '𑌘',
  'ङ': '𑌙',
  'च': '𑌚',
  'छ': '𑌛',
  'ज': '𑌜',
  'झ': '𑌝',
  'ञ': '𑌞',
  'ट': '𑌟',
  'ठ': '𑌠',
  'ड': '𑌡',
  'ढ': '𑌢',
  'ण': '𑌣',
  'त': '𑌤',
  'थ': '𑌥',
  'द': '𑌦',
  'ध': '𑌧',
  'न': '𑌨',
  'प': '𑌪',
  'फ': '𑌫',
  'ब': '𑌬',
  'भ': '𑌭',
  'म': '𑌮',
  'य': '𑌯',
  'र': '𑌰',
  'ल': '𑌲',
  'ळ': '𑌳',
  'व': '𑌵',
  'श': '𑌶',
  'ष': '𑌷',
  'स': '𑌸',
  'ह': '𑌹',
  'ऻ': '𑌻',
  '़': '𑌼',
  'ऽ': '𑌽',
  'ा': '𑌾',
  'ि': '𑌿',
  'ी': '𑍀',
  'ु': '𑍁',
  'ू': '𑍂',
  'ृ': '𑍃',
  'ॄ': '𑍄',
  'े': '𑍇',
  'ै': '𑍈',
  'ो': '𑍋',
  'ौ': '𑍌',
  '्': '𑍍',
  'ॐ': '𑍐',
  'ॗ': '𑍗',
  'ॠ': '𑍠',
  'ॡ': '𑍡',
  'ॢ': '𑍢',
  'ॣ': '𑍣',
  '।': '।',
  '॥': '॥',
  '०': '௦',
  '१': '௧',
  '२': '௨',
  '३': '௩',
  '४': '௪',
  '५': '௫',
  '६': '௬',
  '७': '௭',
  '८': '௮',
  '९': '௯',
};

let to_devanagari = {};
for (let key in to_grantha) {
  to_devanagari[to_grantha[key]] = key;
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
  return chars.join("");
}

function transcribe(node, mapping) {
  if (node.nodeType === Node.TEXT_NODE) {
    console.log(node.nodeValue);
    node.nodeValue = transcribe_string(node.nodeValue, mapping);
  } else if (node.nodeType === Node.ELEMENT_NODE) {
    if (node.lang !== "") return;
    for (let i = 0; i < node.childNodes.length; i++) {
      let child = node.childNodes[i];
      transcribe(child, mapping);
    }
  }
}

function grantha() {
  transcribe(document.body, to_grantha);
  document.title = transcribe_string(document.title, to_grantha);
  document.getElementById("grantha").disabled = true;
  document.getElementById("devanagari").disabled = false;
}

function devanagari() {
  transcribe(document.body, to_devanagari);
  document.title = transcribe_string(document.title, to_devanagari);
  document.getElementById("grantha").disabled = false;
  document.getElementById("devanagari").disabled = true;
}

