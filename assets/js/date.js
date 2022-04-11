let months_accusative = [
    '',
    'Jānuāriās',
    'Februāriās',
    'Mārtiās',
    'Aprīlēs',
    'Maiās',
    'Jūniās',
    'Jūlia',
    'Augustās',
    'Septembrēs',
    'Octōbrēs',
    'Novembrēs',
    'Decembrēs',
    'Jānuāriās',
];
let months_ablative = [
    '',
    'Jānuāriīs',
    'Februāriīs',
    'Mārtiīs',
    'Aprīlēs',
    'Maiīs',
    'Jūniīs',
    'Jūlia',
    'Augustīs',
    'Septembrēs',
    'Octōbrēs',
    'Novembrēs',
    'Decembrēs',
    'Jānuāriīs',
];

let numbers_accusative = [
    'nihil',
    'prīmum',
    'secundum',
    'tertium',
    'quārtum',
    'quīntum',
    'sextum',
    'septimum',
    'octāvum',
    'nōnum',
    'decimum',
    'ūndecimum',
    'duodecimum',
    'tertium decimum',
    'quārtum decimum',
    'quīntum decimum',
    'sextum decimum',
    'septimum decimum',
    'duodēvicēsimus',
    'ūndēvīcēsimus',
    'vīcēsimus',
];

let months_short = [
    '',
    'Iān.',
    'Feb.',
    'Mār.',
    'Apr.',
    'Mai.',
    'Iūn.',
    'Iūl.',
    'Aug.',
    'Sept.',
    'Oct.',
    'Nov.',
    'Dec.',
    'Iān.',
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
        return `kalendīs ${months_ablative[month]}`;
    }

    if (day > kalends && day < nones - 1) {
        return `ante diem ${numbers_accusative[nones - day + 1]} nōnās ${
            months_accusative[month]
        }`;
    }

    if (day == nones - 1) {
        return `prīdiē nōnas ${months_accusative[month]}`;
    }

    if (day == nones) {
        return `nōnīs ${months_ablative[month]}`;
    }

    if (day > nones && day < ides - 1) {
        return `ante diem ${numbers_accusative[ides - day + 1]} īdūs ${
            months_accusative[month]
        }`;
    }

    if (day == ides - 1) {
        return `prīdiē īdūs ${months_accusative[month]}`;
    }

    if (day == ides) {
        return `īdibus ${months_ablative[month]}`;
    }

    if (day > ides && day < last_day_of_month) {
        return `ante diem ${
            numbers_accusative[last_day_of_month + 1 - day + 1]
        } Kalendas ${months_accusative[month + 1]}`;
    }

    if (day == last_day_of_month) {
        return `prīdiē kalendās ${months_accusative[month + 1]}`;
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
        return 'kal. ' + months_short[month];
    }

    if (day > kalends && day < nones - 1) {
        return (
            'a.d. ' +
            numbers_short[nones - day + 1] +
            ' nōn. ' +
            months_short[month]
        );
    }

    if (day == nones - 1) {
        return 'prīd. nōn. ' + months_short[month];
    }

    if (day == nones) {
        return 'nōn. ' + months_short[month];
    }

    if (day > nones && day < ides - 1) {
        return (
            'a.d. ' +
            numbers_short[ides - day + 1] +
            ' eīd. ' +
            months_short[month]
        );
    }

    if (day == ides - 1) {
        return 'prīd. eīd. ' + months_short[month];
    }

    if (day == ides) {
        return 'eīd. ' + months_short[month];
    }

    if (day > ides && day < lastDayOfMonth) {
        return (
            'a.d. ' +
            numbers_short[lastDayOfMonth + 1 - day + 1] +
            ' kal. ' +
            months_short[month + 1]
        );
    }

    if (day == lastDayOfMonth) {
        return 'prīd. kal. ' + months_short[month + 1];
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
