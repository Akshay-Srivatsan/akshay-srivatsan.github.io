let replacement_words = {
    akshay: 'Akshay',
    sreevadhsan: 'Srivatsan',
    ɕɾiːʋadsan: 'ɕɾiːʋatsan',
    श्रीवत्सऩ्: 'श्रीवत्सन्',
    kanini: 'ganini',
    kaɳini: 'gaɳini',
    robaadigs: 'robotics',
    robaadai: 'robotai',
    robaadu: 'robot',
    sdaanfordu: 'Stanford',
    sdaanford: 'Stanford',
    menlo: 'Menlo',
    sgool: 'School',
    insdaagiraam: 'Instagram',
    fesbukku: 'Facebook',
    yunivarsittiyil: 'Universityil',
    kaardaa: 'Carta',
    sdejgaasdu: 'Stagecast',
    aaguvaa: 'Aqua',
    saattelaittu: 'Satellite',
    thaumas: 'Thaumas',
    kidhubu: 'GitHub',
    kidhub: 'GitHub',
    rebaasidoriy: 'repository',
    lingadin: 'LinkedIn',
    yoodyoobu: 'YouTube',
    aangila: 'Aangila',
    lattheen: 'Latin',
    samsgirudha: 'Samskritha',
    thamizh: 'Tamil',
    hindhi: 'Hindi',
    piranj: 'French',
    kandubidi: 'kandupidi',
    maars: 'March',
    ebral: 'April',
};

function transcribe_string(s, map) {
    s = ' ' + s;
    let startingCharacters = ['(', '—', '-', '"', '“'];
    for (let i = 0; i < startingCharacters.length; i++) {
        s = s.replaceAll(startingCharacters[i], startingCharacters[i] + ' ');
    }
    let result = real_transcribe_string(s, map);
    for (let i = startingCharacters.length - 1; i >= 0; i--) {
        result = result.replaceAll(
            startingCharacters[i] + ' ',
            startingCharacters[i]
        );
    }
    result = result.substring(1);
    for (let key in replacement_words) {
        result = result.replaceAll(key, replacement_words[key]);
    }

    return result;
}

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
        'english',
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
    document.documentElement.lang = 'ta-Latn';
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
    document.documentElement.lang = 'ta-phonipa';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?ipa'
    );
    set_script('ipa');
}

function english() {
    reset();
    transcribe(mapping.to_english);
    document.documentElement.lang = 'ta-Latn';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?english'
    );
    set_script('english');
}
