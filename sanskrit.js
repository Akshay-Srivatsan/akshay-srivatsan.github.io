function set_script(name) {
  document.getElementById("grantha-name").style.display = "inline";
  document.getElementById("devanagari-name").style.display = "inline";
  document.getElementById("brahmi-name").style.display = "inline";
  document.getElementById("tamil-name").style.display = "inline";
  document.getElementById("iso-name").style.display = "inline";
  document.getElementById("ipa-name").style.display = "inline";

  document.getElementById(name + "-name").style.display = "none";
  script.selectedIndex = ["iso", "devanagari", "grantha", "brahmi", "tamil", "ipa"].indexOf(name);
}

function transcribe(to) {
  transcribe_document("ISO", to);
}

function iso() {
  reset();
  document.documentElement.lang = "sa-Latn";
  window.history.replaceState("", document.title, window.location.pathname);
  set_script("iso");
}

function grantha() {
  reset();
  transcribe("Grantha");
  document.documentElement.lang = "sa-Gran";
  window.history.replaceState("", document.title, window.location.pathname + "?grantha");
  set_script("grantha");
}

function brahmi() {
  reset();
  transcribe("Brahmi");
  document.documentElement.lang = "sa-Brah";
  window.history.replaceState("", document.title, window.location.pathname + "?brahmi");
  set_script("brahmi");
}

function tamil() {
  reset();
  transcribe("Tamil");
  document.documentElement.lang = "sa-Taml";
  window.history.replaceState("", document.title, window.location.pathname + "?tamil");
  set_script("tamil");
}

function devanagari() {
  reset();
  transcribe("Devanagari");
  document.documentElement.lang = "sa";
  window.history.replaceState("", document.title, window.location.pathname + "?devanagari");
  set_script("devanagari");
}

function ipa() {
  reset();
  transcribe("IPA-Sanskrit");
  document.documentElement.lang = "sa-phonipa";
  window.history.replaceState("", document.title, window.location.pathname + "?ipa");
  set_script("ipa");
}
