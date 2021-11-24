function set_script(name) {
  document.getElementById("grantha-name").style.display = "inline";
  document.getElementById("devanagari-name").style.display = "inline";
  document.getElementById("brahmi-name").style.display = "inline";
  document.getElementById("tamil-name").style.display = "inline";
  document.getElementById("iso-name").style.display = "inline";
  document.getElementById("ipa-name").style.display = "inline";

  document.getElementById(name + "-name").style.display = "none";
  script.selectedIndex = ["tamil", "brahmi", "devanagari", "grantha", "iso", "ipa"].indexOf(name);
}

function transcribe(to) {
  transcribe_document("Tamil", to);
}

function tamil() {
  reset();
  document.documentElement.lang = "ta";
  window.history.replaceState("", document.title, window.location.pathname);
  set_script("tamil");
}

function grantha() {
  reset();
  transcribe("Grantha");
  document.documentElement.lang = "ta-Gran";
  window.history.replaceState("", document.title, window.location.pathname + "?grantha");
  set_script("grantha");
}

function brahmi() {
  reset();
  transcribe("Brahmi");
  document.documentElement.lang = "ta-Brah";
  window.history.replaceState("", document.title, window.location.pathname + "?brahmi");
  set_script("brahmi");
}

function devanagari() {
  reset();
  transcribe("Devanagari");
  document.documentElement.lang = "ta-Deva";
  window.history.replaceState("", document.title, window.location.pathname + "?tamil");
  set_script("devanagari");
}

function iso() {
  reset();
  transcribe("ISO");
  document.documentElement.lang = "sa-Latn";
  window.history.replaceState("", document.title, window.location.pathname + "?iso");
  set_script("iso");
}

function ipa() {
  reset();
  transcribe("IPA-Tamil");
  document.documentElement.lang = "sa-phonipa";
  window.history.replaceState("", document.title, window.location.pathname + "?ipa");
  set_script("ipa");
}
