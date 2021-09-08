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

let to_brahmi = {
  'ँ': '𑀀',
  'ं': '𑀁',
  'ः': '𑀂',
  'अ': '𑀅',
  'आ': '𑀆',
  'इ': '𑀇',
  'ई': '𑀈',
  'उ': '𑀉',
  'ऊ': '𑀊',
  'ऋ': '𑀋',
  'ऌ': '𑀍',
  'ए': '𑀏',
  'ऐ': '𑀐',
  'ओ': '𑀑',
  'औ': '𑀒',
  'क': '𑀓',
  'ख': '𑀔',
  'ग': '𑀕',
  'घ': '𑀖',
  'ङ': '𑀗',
  'च': '𑀘',
  'छ': '𑀙',
  'ज': '𑀚',
  'झ': '𑀛',
  'ञ': '𑀜',
  'ट': '𑀝',
  'ठ': '𑀞',
  'ड': '𑀟',
  'ढ': '𑀠',
  'ण': '𑀡',
  'त': '𑀢',
  'थ': '𑀣',
  'द': '𑀤',
  'ध': '𑀥',
  'न': '𑀦',
  'ऩ': '𑀷',
  'प': '𑀧',
  'फ': '𑀨',
  'ब': '𑀩',
  'भ': '𑀪',
  'म': '𑀫',
  'य': '𑀬',
  'र': '𑀭',
  'ऱ': '𑀶',
  'ल': '𑀮',
  'ळ': '𑀴',
  'ऴ': '𑀵',
  'व': '𑀯',
  'श': '𑀰',
  'ष': '𑀱',
  'स': '𑀲',
  'ह': '𑀳',
  'ा': '𑀸',
  'ि': '𑀺',
  'ी': '𑀻',
  'ु': '𑀼',
  'ू': '𑀽',
  'ृ': '𑀾',
  'ॄ': '𑀿',
  'े': '𑁂',
  'ै': '𑁃',
  'ो': '𑁄',
  'ौ': '𑁅',
  '्': '𑁆',
  'ॠ': '𑀌',
  'ॡ': '𑀎',
  'ॢ': '𑁀',
  'ॣ': '𑁁',
  '।': '𑁇',
  '॥': '𑁈',
  '०': '𑁦',
  '१': '𑁧',
  '२': '𑁨',
  '३': '𑁩',
  '४': '𑁪',
  '५': '𑁫',
  '६': '𑁬',
  '७': '𑁭',
  '८': '𑁮',
  '९': '𑁯',
};

let from_grantha = {};
for (let key in to_grantha) {
  from_grantha[to_grantha[key]] = key;
}
let from_brahmi = {};
for (let key in to_brahmi) {
  from_brahmi[to_brahmi[key]] = key;
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
    node.nodeValue = transcribe_string(node.nodeValue, mapping);
  } else if (node.nodeType === Node.ELEMENT_NODE) {
    if (node.lang !== "") return;
    for (let i = 0; i < node.childNodes.length; i++) {
      let child = node.childNodes[i];
      transcribe(child, mapping);
    }
  }
}

function set_script(script) {
  document.getElementById("grantha").disabled = false;
  document.getElementById("devanagari").disabled = false;
  document.getElementById("brahmi").disabled = false;
  document.getElementById("grantha-name").style.display = "inline";
  document.getElementById("devanagari-name").style.display = "inline";
  document.getElementById("brahmi-name").style.display = "inline";

  document.getElementById(script).disabled = true;
  document.getElementById(script + "-name").style.display = "none";
}

function grantha() {
  devanagari();
  transcribe(document.body, to_grantha);
  document.title = transcribe_string(document.title, to_grantha);
  document.documentElement.lang = "sa-Gran";
  window.history.replaceState("", document.title, window.location.pathname + "?grantha");
  set_script("grantha");
}

function brahmi() {
  devanagari();
  transcribe(document.body, to_brahmi);
  document.title = transcribe_string(document.title, to_brahmi);
  document.documentElement.lang = "sa-Gran";
  window.history.replaceState("", document.title, window.location.pathname + "?brahmi");
  set_script("brahmi");
}

function devanagari() {
  transcribe(document.body, from_brahmi);
  transcribe(document.body, from_grantha);
  document.title = transcribe_string(document.title, from_grantha);
  document.title = transcribe_string(document.title, from_brahmi);
  document.documentElement.lang = "sa";
  window.history.replaceState("", document.title, window.location.pathname);
  set_script("devanagari");
}
