let months_accusative = [
    '',
    'Ianuarias',
    'Februarias',
    'Martias',
    'Apriles',
    'Maias',
    'Iunias',
    'Iulias',
    'Augustas',
    'Septembres',
    'Octobres',
    'Novembres',
    'Decembres',
    'Ianuarias',
];
let months_ablative = [
    '',
    'Ianuariis',
    'Februariis',
    'Martiis',
    'Aprilibus',
    'Maiis',
    'Iuniis',
    'Iuliis',
    'Augustis',
    'Septembribus',
    'Octobribus',
    'Novembribus',
    'Decembribus',
    'Ianuariis',
];

let numbers_accusative = [
    'nihil',
    'primum',
    'secundum',
    'tertium',
    'quartum',
    'quintum',
    'sextum',
    'septimum',
    'octavum',
    'nonum',
    'decimum',
    'undecimum',
    'duodecimum',
    'tertium decimum',
    'quartum decimum',
    'quintum decimum',
    'sextum decimum',
    'septimum decimum',
    'duodevicesimum',
    'undevicesimum',
    'vicesimum',
];

let months_short = [
    '',
    'Ian.',
    'Feb.',
    'Mar.',
    'Apr.',
    'Mai.',
    'Iun.',
    'Iul.',
    'Aug.',
    'Sept.',
    'Oct.',
    'Nov.',
    'Dec.',
    'Ian.',
];

let numbers_short = [
    'N',
    'I',
    'II',
    'III',
    'IV',
    'V',
    'VI',
    'VII',
    'VIII',
    'IX',
    'X',
    'XI',
    'XII',
    'XIII',
    'XIV',
    'XV',
    'XVI',
    'XVII',
    'XVIII',
    'XIX',
    'XX',
];

function roman_date(date) {
    let day = date.getDate();
    let month = date.getMonth() + 1;
    let year = date.getYear();

    let last_day_of_month = new Date(year, month, 0).getDate();

    let kalends = 1;
    let nones = 5;
    let ides = 13;
    if ([3, 5, 7, 10].indexOf(month) != -1) {
        nones = 7;
        ides = 15;
    }

    if (day == kalends) {
        return `Kalendis ${months_ablative[month]}`;
    }

    if (day > kalends && day < nones - 1) {
        return `ante diem ${numbers_accusative[nones - day + 1]} Nonas ${
            months_accusative[month]
        }`;
    }

    if (day == nones - 1) {
        return `pridie Nonas ${months_accusative[month]}`;
    }

    if (day == nones) {
        return `Nonis ${months_ablative[month]}`;
    }

    if (day > nones && day < ides - 1) {
        return `ante diem ${numbers_accusative[ides - day + 1]} Idus ${
            months_accusative[month]
        }`;
    }

    if (day == ides - 1) {
        return `pridie Idus ${months_accusative[month]}`;
    }

    if (day == ides) {
        return `Idibus ${months_ablative[month]}`;
    }

    if (day > ides && day < last_day_of_month) {
        return `ante diem ${
            numbers_accusative[last_day_of_month + 1 - day + 1]
        } Kalendas ${months_accusative[month + 1]}`;
    }

    if (day == last_day_of_month) {
        return `pridie Kalendas ${months_accusative[month + 1]}`;
    }
}

function shortRomanDate(date) {
    let day = date.getDate();
    let month = date.getMonth() + 1;
    let year = date.getYear();

    let lastDayOfMonth = new Date(year, month, 0).getDate();

    let kalends = 1;
    let nones = 5;
    let ides = 13;
    if ([3, 5, 7, 10].indexOf(month) != -1) {
        nones = 7;
        ides = 15;
    }

    if (day == kalends) {
        return 'Kal. ' + months_short[month];
    }

    if (day > kalends && day < nones - 1) {
        return (
            'a.d. ' +
            numbers_short[nones - day + 1] +
            ' Non. ' +
            months_short[month]
        );
    }

    if (day == nones - 1) {
        return 'Prid. Non. ' + months_short[month];
    }

    if (day == nones) {
        return 'Non. ' + months_short[month];
    }

    if (day > nones && day < ides - 1) {
        return (
            'a.d. ' +
            numbers_short[ides - day + 1] +
            ' Eid. ' +
            months_short[month]
        );
    }

    if (day == ides - 1) {
        return 'Prid. Eid. ' + months_short[month];
    }

    if (day == ides) {
        return 'Eid. ' + months_short[month];
    }

    if (day > ides && day < lastDayOfMonth) {
        return (
            'a.d. ' +
            numbers_short[lastDayOfMonth + 1 - day + 1] +
            ' Kal. ' +
            months_short[month + 1]
        );
    }

    if (day == lastDayOfMonth) {
        return 'Prid. Kal. ' + months_short[month + 1];
    }
}

function romanize(string) {
    return string
        .replace(new RegExp('U', 'g'), 'V')
        .replace(new RegExp('v', 'g'), 'u');
}

function hodie() {
    return roman_date(new Date());
}

function hodieBreve() {
    return shortRomanDate(new Date());
}
