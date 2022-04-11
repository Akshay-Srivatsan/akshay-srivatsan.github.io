function changeFont(type) {
    if (type && type[0] === '?') type = type.substr(1);
    if (type == 'uncials') uncials();
    else if (type == 'capitals') capitals();
    else if (type == 'italics') italics();
}

function capitals() {
    reset();
    change_font.selectedIndex = 0;
    document.documentElement.lang = 'la';
    window.history.replaceState('', document.title, window.location.pathname);
}

function uncials() {
    reset();
    transcribe_document(mapping.to_ascii);
    change_font.selectedIndex = 1;
    document.documentElement.lang = 'la-Latg';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?uncials'
    );
}

function italics() {
    reset();
    transcribe_document(mapping.to_italics);
    change_font.selectedIndex = 2;
    document.documentElement.lang = 'la-Ital';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?italics'
    );
}
