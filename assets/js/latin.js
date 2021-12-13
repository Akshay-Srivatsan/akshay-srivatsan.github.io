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
    change_font.selectedIndex = 1;
    document.documentElement.lang = 'la-Latg';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?uncials'
    );
}

let latn = [
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    ' ',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
];
let ital = [
    '𐌀',
    '𐌁',
    '𐌂',
    '𐌃',
    '𐌄',
    '𐌅',
    '𐌂',
    '𐌇',
    '𐌉',
    '𐌉',
    '𐌊',
    '𐌋',
    '𐌌',
    '𐌍',
    '𐌏',
    '𐌐',
    '𐌒',
    '𐌓',
    '𐌔',
    '𐌕',
    '𐌖',
    '𐌖',
    '𐌞',
    '𐌗',
    '𐌖',
    '𐌆',
    ' ',
    '𐌀',
    '𐌁',
    '𐌂',
    '𐌃',
    '𐌄',
    '𐌅',
    '𐌂',
    '𐌇',
    '𐌉',
    '𐌉',
    '𐌊',
    '𐌋',
    '𐌌',
    '𐌍',
    '𐌏',
    '𐌐',
    '𐌒',
    '𐌓',
    '𐌔',
    '𐌕',
    '𐌖',
    '𐌖',
    '𐌞',
    '𐌗',
    '𐌖',
    '𐌆',
];
let to_italics = Object.fromEntries(latn.map((k, i) => [k, ital[i]]));

function italics() {
    reset();
    transcribe_document(to_italics);
    change_font.selectedIndex = 2;
    document.documentElement.lang = 'la-Ital';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?italics'
    );
}
