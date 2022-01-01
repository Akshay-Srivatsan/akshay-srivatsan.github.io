function set_script(name) {
    document.getElementById('grantha-name').style.display = 'inline';
    document.getElementById('devanagari-name').style.display = 'inline';
    document.getElementById('brahmi-name').style.display = 'inline';
    document.getElementById('tamil-name').style.display = 'inline';
    document.getElementById('iso-name').style.display = 'inline';
    document.getElementById('ipa-name').style.display = 'inline';

    document.getElementById(name + '-name').style.display = 'none';
    script.selectedIndex = [
        'tamil',
        'brahmi',
        'devanagari',
        'iso',
        'ipa',
    ].indexOf(name);
}

function transcribe(to) {
    transcribe_document(to);
}

function tamil() {
    reset();
    document.documentElement.lang = 'ta';
    window.history.replaceState('', document.title, window.location.pathname);
    set_script('tamil');
}

function brahmi() {
    reset();
    transcribe(mapping.to_brahmi);
    document.documentElement.lang = 'ta-Brah';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?brahmi'
    );
    set_script('brahmi');
}

function devanagari() {
    reset();
    transcribe(mapping.to_devanagari);
    document.documentElement.lang = 'ta-Deva';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?devanagari'
    );
    set_script('devanagari');
}

function iso() {
    reset();
    transcribe(mapping.to_iso);
    document.documentElement.lang = 'sa-Latn';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?iso'
    );
    set_script('iso');
}

function ipa() {
    reset();
    transcribe(mapping.to_ipa);
    document.documentElement.lang = 'sa-phonipa';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?ipa'
    );
    set_script('ipa');
}
