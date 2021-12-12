var fenestra = window;
fenestra.documentum = document;
fenestra.documentum.scribe = document.write;
fenestra.locus = window.location;
fenestra.locus.quaesitum = window.location.search;

function aliasProperty(type, old, alias, setter) {
    let obj = {};
    obj[alias] = {
        get: function () {
            return this[old];
        },
    };
    if (setter) {
        obj[alias]['set'] = function (val) {
            this[old] = val;
        };
    }
    type.prototype = Object.defineProperties(type.prototype, obj);
}

aliasProperty(HTMLDivElement, 'style', 'aspectus');
aliasProperty(CSS2Properties, 'display', 'forma', true);

var TALEA = 'block';

function mutaSpeciem(type) {
    if (type && type[0] === '?') type = type.substr(1);
    if (type == 'unciales') unciales();
    else if (type == 'capitales') capitales();
    else if (type == 'italica') italica();
}

function capitales() {
    reset();
    speciesElige.selectedIndex = 0;
    document.documentElement.lang = 'la';
    window.history.replaceState('', document.title, window.location.pathname);
}

function unciales() {
    reset();
    speciesElige.selectedIndex = 1;
    document.documentElement.lang = 'la-Latg';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?unciales'
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

function italica() {
    reset();
    transcribe_document(to_italics);
    speciesElige.selectedIndex = 2;
    document.documentElement.lang = 'la-Ital';
    window.history.replaceState(
        '',
        document.title,
        window.location.pathname + '?italica'
    );
}
